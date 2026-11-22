#lang racket

;; ============================================================================
;; ELF 文件包装器 (ELF file wrapper)
;; ============================================================================
;; 依据 ruleset/elf/elf-file.scm 中的布局规则,把代码段、数据段、符号表与
;; 重定位信息打包为 ELF32/ELF64 文件:
;;   - wrap-executable : 生成可执行文件 (ET_EXEC,带程序头),对应 issue #8
;;   - wrap-object     : 生成可重定位目标文件 (ET_REL,带重定位项),对应 issue #9
;; 只使用基础 Scheme/Racket 过程,兼容 Racket 7.5。

(require racket/list)
(require racket/runtime-path)
(require racket/string)

(provide read-elf-rules
         default-elf-rules-path
         rules-ref
         struct-size
         code-strings->bytes
         make-string-table
         elf-section
         text-section
         data-section
         bss-section
         elf-symbol
         make-elf-symbol
         elf-relocation
         make-relocation
         symbol-info
         wrap-executable
         wrap-object
         write-elf-file
         elf-memory-layout)

;; 规则文件位于仓库根目录的 ruleset/elf/elf-file.scm。
;; 本模块位于 src/wrapper/,因此向上两级到达仓库根目录。
(define-runtime-path default-elf-rules-path
  (build-path ".." ".." "ruleset" "elf" "elf-file.scm"))

;; ---------------------------------------------------------------------------
;; 常量
;; ---------------------------------------------------------------------------
(define SHN_UNDEF 0)
(define SHN_ABS   #xfff1)

(define STB_LOCAL  0)
(define STB_GLOBAL 1)
(define STB_WEAK   2)

(define STT_NOTYPE  0)
(define STT_OBJECT  1)
(define STT_FUNC    2)
(define STT_SECTION 3)
(define STT_FILE    4)

(define SHF_WRITE     1)
(define SHF_ALLOC     2)
(define SHF_EXECINSTR 4)

(define PF_X 1)
(define PF_W 2)
(define PF_R 4)

;; ---------------------------------------------------------------------------
;; 基础工具
;; ---------------------------------------------------------------------------

;; 向上对齐 n 到 a 的倍数。
(define (align-up n a)
  (if (or (not (integer? a)) (<= a 1))
      n
      (* a (quotient (+ n a -1) a))))

;; 在关联表中查找 key 的值,找不到返回 default。
(define (alist-value key alst [default #f])
  (let ([pair (assoc key alst)])
    (if pair (cdr pair) default)))

;; 在属性列表 ((key value) ...) 中查找 key 的值。
(define (plist-value key plst [default #f])
  (let ([pair (assoc key plst)])
    (if pair (cadr pair) default)))

;; 取属性列表中 key 对应的「剩余元素」列表,如 (flags r x) -> (r x)。
(define (plist-rest key plst [default '()])
  (let ([pair (assoc key plst)])
    (if pair (cdr pair) default)))

;; 在列表中查找满足谓词的第一个元素。
(define (find-first pred xs)
  (cond [(null? xs) #f]
        [(pred (car xs)) (car xs)]
        [else (find-first pred (cdr xs))]))

;; ---------------------------------------------------------------------------
;; 规则读取与查询
;; ---------------------------------------------------------------------------

;; 读入 ELF 布局规则(纯 S-expression 数据)。
(define (read-elf-rules [path (path->string default-elf-rules-path)])
  (call-with-input-file path (lambda (in) (read in))))

;; 按名字取得规则中的一节,例如 (rules-ref rules 'header)。
(define (rules-ref rules name)
  (find-first (lambda (section)
                (and (pair? section) (eq? (car section) name)))
              rules))

;; 取得某节的定义字段(去掉节名)。
(define (rules-fields rules name)
  (let ([section (rules-ref rules name)])
    (if section (cdr section) '())))

;; 展开 (class-fields ((32 (...)) (64 (...)))),按 class 选择字段列表。
(define (flatten-class-fields fields class)
  (apply append
         (map (lambda (field)
                (if (and (pair? field) (eq? (car field) 'class-fields))
                    (let ([chosen (assoc class (second field))])
                      (if chosen
                          (second chosen)
                          (error 'flatten-class-fields
                                 "no field layout for class ~a" class)))
                    (list field)))
              fields)))

;; 取得某个结构的全部字段(按 class 展开)。
(define (struct-fields rules name class)
  (flatten-class-fields (rules-fields rules name) class))

;; 计算字段宽度(字节数)。
(define (field-width spec class)
  (cond [(integer? spec) spec]
        [(equal? spec '(word)) (if (= class 32) 4 8)]
        [(equal? spec '(half)) 2]
        [(equal? spec '(full)) 4]
        [else (error 'field-width "unknown width spec: ~s" spec)]))

;; 按规则计算结构大小(用于填充 header 的 sizeof 字段)。
(define (struct-size rules name class)
  (apply + (map (lambda (field) (field-width (second field) class))
                (struct-fields rules name class))))

;; ---------------------------------------------------------------------------
;; 注解处理:常量、枚举、来源
;; ---------------------------------------------------------------------------

(define (find-annotation pred annotations)
  (cond [(null? annotations) #f]
        [(pred (car annotations)) (car annotations)]
        [else (find-annotation pred (cdr annotations))]))

(define (constant-annotation annotations)
  (find-annotation (lambda (a) (and (pair? a) (eq? (car a) '=))) annotations))

(define (enum-annotation annotations)
  (find-annotation (lambda (a) (and (pair? a) (pair? (car a)))) annotations))

;; 枚举表中的原始值(raw 写入文件)与可读值(decoded)互查。
(define (enum-raw pairs v)
  (let loop ([ps pairs])
    (cond [(null? ps) v]
          [(or (equal? v (caar ps)) (equal? v (cadar ps))) (caar ps)]
          [else (loop (cdr ps))])))

(define (enum-decoded pairs v)
  (let loop ([ps pairs])
    (cond [(null? ps) v]
          [(or (equal? v (caar ps)) (equal? v (cadar ps))) (cadar ps)]
          [else (loop (cdr ps))])))

;; 从 fileinfo 中取值:field 为 'class 或 'data,value 为 32/64 或 little/big。
;; 返回枚举中的 decoded 值(即写入 ELF 头的常量)。
(define (fileinfo-decoded rules field value)
  (let* ([section (rules-ref rules 'fileinfo)]
         [def (find-first (lambda (f) (and (pair? f) (eq? (car f) field)))
                          (cdr section))])
    (enum-decoded (third def) value)))

;; ---------------------------------------------------------------------------
;; 字段序列化(由规则驱动)
;; ---------------------------------------------------------------------------

;; 将单个值编码为指定宽度的字节。
(define (encode-value value width endian)
  (cond [(and (list? value) (andmap integer? value))
         (list->bytes value)]
        [(bytes? value) value]
        [(string? value) (string->bytes/utf-8 value)]
        [else
         ;; 手工按字节编码,支持任意宽度(integer->integer-bytes 仅接受 1/2/4/8)。
         (let ([little-bytes
                (let loop ([i 0] [acc '()])
                  (if (= i width)
                      (reverse acc)
                      (loop (+ i 1)
                            (cons (bitwise-and
                                   (arithmetic-shift value (* -8 i)) 255)
                                  acc))))])
           (list->bytes (if (eq? endian 'big)
                            (reverse little-bytes)
                            little-bytes)))]))

;; 计算字段最终写入的值。
(define (field-value name annotations env rules)
  (let ([constant (constant-annotation annotations)]
        [enum (enum-annotation annotations)])
    (let* ([value (if constant
                      (cadr constant)
                      (alist-value name env))]
           [value (if (and enum (not (and constant
                                          (list? value))))
                      (enum-raw enum value)
                      value)])
      (when (not value)
        (error 'field-value "missing value for field ~a" name))
      value)))

;; 按规则字段顺序把一个结构的环境(关联表)序列化为字节。
(define (serialize-struct rules struct-name class endian env)
  (apply bytes-append
         (map (lambda (field)
                (let ([name (first field)]
                      [width (field-width (second field) class)]
                      [annotations (cddr field)])
                  (encode-value (field-value name annotations env rules)
                                width endian)))
              (struct-fields rules struct-name class))))

;; ---------------------------------------------------------------------------
;; 输入数据结构
;; ---------------------------------------------------------------------------

;; 节:content 为字节串(有内容)或整数(.bss 的大小)。
(struct elf-section (name type flags addr content link info entsize align)
        #:transparent)

(define (text-section bytes #:addr [addr 0] #:align [align 4])
  (elf-section "text" 'progbits (list 'alloc 'execinstr) addr bytes 0 0 0 align))

(define (data-section bytes #:addr [addr 0] #:align [align 4])
  (elf-section "data" 'progbits (list 'alloc 'write) addr bytes 0 0 0 align))

(define (bss-section size #:addr [addr 0] #:align [align 4])
  (elf-section "bss" 'nobits (list 'alloc 'write) addr size 0 0 0 align))

;; 符号:name 为字符串,value/size 为数值,bind/type/shndx 为可读符号或数值。
(struct elf-symbol (name value size bind type shndx) #:transparent)

(define (make-elf-symbol name
                         #:value [value 0]
                         #:size [size 0]
                         #:bind [bind 'local]
                         #:type [type 'notype]
                         #:shndx [shndx 'text])
  (elf-symbol name value size bind type shndx))

;; 重定位项:symbol 可以是符号名字符串或符号在符号表中的索引。
(struct elf-relocation (offset symbol type addend) #:transparent)

(define (make-relocation offset symbol #:type [type 0] #:addend [addend 0])
  (elf-relocation offset symbol type addend))

;; 把可读的 bind/type 转换为 st_info 字节。
(define (symbol-info symbol)
  (define bind (elf-symbol-bind symbol))
  (define type (elf-symbol-type symbol))
  (define bind-value
    (cond [(integer? bind) bind]
          [(eq? bind 'global) STB_GLOBAL]
          [(eq? bind 'weak) STB_WEAK]
          [else STB_LOCAL]))
  (define type-value
    (cond [(integer? type) type]
          [(eq? type 'func) STT_FUNC]
          [(eq? type 'object) STT_OBJECT]
          [(eq? type 'section) STT_SECTION]
          [(eq? type 'file) STT_FILE]
          [else STT_NOTYPE]))
  (bitwise-ior (arithmetic-shift bind-value 4) type-value))

;; ---------------------------------------------------------------------------
;; 字节/字符串表
;; ---------------------------------------------------------------------------

;; 把 32 位二进制字符串列表转换为小端字节串。
(define (code-strings->bytes strings)
  (apply bytes-append
         (map (lambda (s)
                (if (or (string=? s "")
                        (not (regexp-match? #px"^[01]+$" s)))
                    (bytes)
                    (integer->integer-bytes (string->number s 2) 4 #f #f)))
              strings)))

;; 生成以 NUL 开头、每项以 NUL 结尾的字符串表,返回 (bytes . offsets)。
;; offsets 为 name -> 在表中的偏移。
(define (make-string-table names)
  (let loop ([names names] [pos 1] [parts (list #"\0")] [offsets '()])
    (if (null? names)
        (cons (apply bytes-append (reverse parts)) (reverse offsets))
        (let* ([name (car names)]
               [bytes (string->bytes/utf-8 name)])
          (loop (cdr names)
                (+ pos (bytes-length bytes) 1)
                (cons (bytes-append bytes #"\0") parts)
                (cons (cons name pos) offsets))))))

;; ---------------------------------------------------------------------------
;; 节名与节类型
;; ---------------------------------------------------------------------------

;; 内部节名 -> ELF 节名(带点号)。
(define (section-display-name name)
  (cond [(string=? name "null") ""]
        [(string-prefix? name ".") name]
        [else (string-append "." name)]))

;; 从规则的 sections 定义中读取某节的类型(返回可读符号)。
(define (section-rule-type rules name)
  (let* ([def (rules-ref rules 'sections)]
         [entry (find-first (lambda (s) (and (pair? s) (eq? (car s) (string->symbol name))))
                            (cdr def))])
    (if entry
        (plist-value 'type (cdr entry) (string->symbol name))
        (string->symbol name))))

;; 从规则的 sections 定义中读取某节的标志(返回可读符号列表)。
(define (section-rule-flags rules name)
  (let* ([def (rules-ref rules 'sections)]
         [entry (find-first (lambda (s) (and (pair? s) (eq? (car s) (string->symbol name))))
                            (cdr def))])
    (if entry (plist-rest 'flags (cdr entry) '()) '())))

;; 从规则的 segments 定义中读取段的标志(返回可读符号列表)。
(define (segment-rule-flags rules name)
  (let* ([def (rules-ref rules 'segments)]
         [entry (find-first (lambda (s) (and (pair? s) (eq? (car s) name)))
                            (cdr def))])
    (if entry (plist-rest 'flags (cdr entry) '()) '())))

;; 把标志符号列表转换为位掩码。
(define (flags-mask flags table)
  (apply bitwise-ior
         (map (lambda (f)
                (cond [(integer? f) f]
                      [else (alist-value f table 0)]))
              flags)))

(define shf-table
  (list (cons 'write SHF_WRITE)
        (cons 'alloc SHF_ALLOC)
        (cons 'execinstr SHF_EXECINSTR)))

(define pf-table
  (list (cons 'r PF_R) (cons 'w PF_W) (cons 'x PF_X)))

;; 符号 shndx:节名 -> 在节列表中的索引,特殊符号 -> 常量。
(define (resolve-shndx shndx sections names)
  (cond [(integer? shndx) shndx]
        [(eq? shndx 'undef) SHN_UNDEF]
        [(eq? shndx 'abs) SHN_ABS]
        [(symbol? shndx)
         (let* ([name (symbol->string shndx)]
                [index (find-first
                        (lambda (i) (string=? (list-ref names i) name))
                        (range (length names)))])
           (if index index (length names)))]
        [(string? shndx)
         (let ([index (find-first
                       (lambda (i) (string=? (list-ref names i) shndx))
                       (range (length names)))])
           (if index index (length names)))]
        [else SHN_UNDEF]))

;; ---------------------------------------------------------------------------
;; 核心构建
;; ---------------------------------------------------------------------------

;; 根据规则与输入构建 ELF 字节串。
;; 内部函数,同时返回布局信息供 elf-memory-layout 使用。
(define (build-elf
         #:rules [rules (read-elf-rules)]
         #:class [class 32]
         #:endian [endian 'little]
         #:type [type 'exec]
         #:machine [machine 'riscv]
         #:sections [input-sections '()]
         #:symbols [input-symbols '()]
         #:relocations [input-relocations '()]
         #:load-addr [load-addr #x80000000]
         #:entry [entry '_start]
         #:align [align #x1000])
  ;; ---- 基本参数 -----------------------------------------------------------
  (when (not (member class '(32 64)))
    (error 'build-elf "class must be 32 or 64, got ~a" class))
  (define word (if (= class 32) 4 8))
  (define class-value (fileinfo-decoded rules 'class class))
  (define data-value (fileinfo-decoded rules 'data endian))
  (define (header-field-enum field)
    (caddr (find-first (lambda (f) (and (pair? f) (eq? (car f) field)))
                       (rules-fields rules 'header))))
  (define type-value (enum-raw (header-field-enum 'type) type))
  (define machine-value (enum-raw (header-field-enum 'machine) machine))

  (define executable? (eq? type 'exec))
  (define relocatable? (eq? type 'rel))

  ;; ---- 整理输入节 ---------------------------------------------------------
  (define (get-section name)
    (find-first (lambda (s)
                  (or (string=? (elf-section-name s) name)
                      (string=? (elf-section-name s) (string-append "." name))))
                input-sections))
  (define text (or (get-section "text")
                   (text-section (bytes))))
  (define data (get-section "data"))
  (define bss (get-section "bss"))

  (define text-bytes (elf-section-content text))
  (define data-bytes (if data (elf-section-content data) (bytes)))
  (define bss-size (if bss (elf-section-content bss) 0))
  (define text-size (bytes-length text-bytes))
  (define data-size (bytes-length data-bytes))

  (define has-data? (> data-size 0))
  (define has-bss? (and (integer? bss-size) (> bss-size 0)))
  (define has-relocations? (and relocatable? (pair? input-relocations)))

  ;; ---- 节顺序 -------------------------------------------------------------
  ;; null 恒定存在;text 恒定存在;data/bss 按需;其余由包装器生成。
  (define section-order
    (append (list "null" "text")
            (if has-data? (list "data") '())
            (if has-bss? (list "bss") '())
            (list "shstrtab" "strtab" "symtab")
            (if has-relocations? (list "rela.text") '())))
  (define section-names
    (map section-display-name section-order))
  (define section-count (length section-order))

  ;; ---- 字符串表 -----------------------------------------------------------
  (define shstrtab (make-string-table section-names))
  (define shstrtab-bytes (car shstrtab))
  (define shstrtab-offsets (cdr shstrtab))

  (define symbol-names
    (cons "" (map (lambda (s) (if (symbol? (elf-symbol-name s))
                                  (symbol->string (elf-symbol-name s))
                                  (elf-symbol-name s)))
                  input-symbols)))
  (define strtab (make-string-table symbol-names))
  (define strtab-bytes (car strtab))
  (define strtab-offsets (cdr strtab))

  ;; ---- 符号表 -------------------------------------------------------------
  (define null-symbol-bytes (make-bytes (struct-size rules 'symbol class) 0))
  (define symbol-index-table
    (let loop ([symbols input-symbols] [index 1] [acc '()])
      (if (null? symbols)
          (reverse acc)
          (let ([name (if (symbol? (elf-symbol-name (car symbols)))
                          (symbol->string (elf-symbol-name (car symbols)))
                          (elf-symbol-name (car symbols)))])
            (loop (cdr symbols) (+ index 1)
                  (cons (cons name index) acc))))))

  (define symbol-entries
    (map (lambda (symbol)
           (let* ([name (if (symbol? (elf-symbol-name symbol))
                            (symbol->string (elf-symbol-name symbol))
                            (elf-symbol-name symbol))]
                  [env (list
                        (cons 'name (alist-value name strtab-offsets 0))
                        (cons 'value (elf-symbol-value symbol))
                        (cons 'size (elf-symbol-size symbol))
                        (cons 'info (symbol-info symbol))
                        (cons 'other 0)
                        (cons 'shndx (resolve-shndx (elf-symbol-shndx symbol)
                                                    input-sections
                                                    section-order)))])
             (serialize-struct rules 'symbol class endian env)))
         input-symbols))
  (define symtab-bytes
    (apply bytes-append (cons null-symbol-bytes symbol-entries)))

  ;; ---- 重定位表 -----------------------------------------------------------
  (define relocation-bytes
    (if has-relocations?
        (apply bytes-append
               (map (lambda (rel)
                      (let* ([target (elf-relocation-symbol rel)]
                             [index (cond [(integer? target) target]
                                          [(symbol? target)
                                           (alist-value (symbol->string target)
                                                        symbol-index-table 0)]
                                          [else (alist-value target
                                                             symbol-index-table 0)])]
                            [shift (if (= class 32) 8 32)]
                            [info (bitwise-ior
                                   (arithmetic-shift index shift)
                                   (elf-relocation-type rel))]
                            [env (list
                                  (cons 'offset (elf-relocation-offset rel))
                                  (cons 'info info)
                                  (cons 'addend (elf-relocation-addend rel)))])
                       (serialize-struct rules 'relocation class endian env)))
                    input-relocations))
        (bytes)))

  ;; ---- 结构大小 -----------------------------------------------------------
  (define ehsize (struct-size rules 'header class))
  (define phentsize (struct-size rules 'program-header class))
  (define shentsize (struct-size rules 'section-header class))
  (define nph (if executable?
                  (if (or has-data? has-bss?) 2 1)
                  0))
  (define phnum nph)
  (define phoff (if (> phnum 0) ehsize 0))
  (define headers-end (+ ehsize (* phnum phentsize)))

  ;; ---- 文件布局 -----------------------------------------------------------
  (define text-offset (align-up headers-end align))
  ;; 可执行文件:text 的虚拟地址就是 load-addr。
  (define text-vaddr* (if executable? load-addr 0))
  (define data-offset (align-up (+ text-offset text-size) word))
  (define data-vaddr (if executable?
                         (+ load-addr (- data-offset text-offset))
                         0))
  (define bss-offset (+ data-offset data-size))
  (define bss-vaddr (if executable?
                        (+ load-addr (- bss-offset text-offset))
                        0))
  (define content-end bss-offset)
  (define shoff (align-up content-end word))
  (define shstrtab-offset (+ shoff (* section-count shentsize)))
  (define strtab-offset (+ shstrtab-offset (bytes-length shstrtab-bytes)))
  (define symtab-offset (+ strtab-offset (bytes-length strtab-bytes)))
  (define rela-offset (+ symtab-offset (bytes-length symtab-bytes)))
  (define file-end (+ rela-offset (bytes-length relocation-bytes)))

  ;; ---- 入口点 -------------------------------------------------------------
  (define entry-address
    (cond [(number? entry) entry]
          [(not entry) (if executable? load-addr 0)]
          [(or (symbol? entry) (string? entry))
           (let* ([name (if (symbol? entry)
                            (symbol->string entry)
                            entry)]
                  [found (find-first
                          (lambda (s)
                            (string=? name
                                      (if (symbol? (elf-symbol-name s))
                                          (symbol->string (elf-symbol-name s))
                                          (elf-symbol-name s))))
                          input-symbols)])
             (if found
                 (+ (if executable? load-addr 0)
                    (elf-symbol-value found))
                 (if executable? load-addr 0)))]
          [else (if executable? load-addr 0)]))

  ;; ---- 文件头 -------------------------------------------------------------
  (define header-env
    (list
     (cons 'class class-value)
     (cons 'data data-value)
     (cons 'type type-value)
     (cons 'machine machine-value)
     (cons 'entry entry-address)
     (cons 'phoff phoff)
     (cons 'shoff shoff)
     (cons 'ehsize ehsize)
     (cons 'phentsize phentsize)
     (cons 'phnum phnum)
     (cons 'shentsize shentsize)
     (cons 'shnum section-count)
     (cons 'shstrndx
           (find-first (lambda (i) (string=? (list-ref section-order i) "shstrtab"))
                       (range section-count)))))
  (define header-bytes
    (serialize-struct rules 'header class endian header-env))

  ;; ---- 程序头 -------------------------------------------------------------
  (define (program-header-env name offset vaddr filesz memsz)
    (let ([flags (flags-mask (segment-rule-flags rules (string->symbol name))
                            pf-table)])
      (list
       (cons 'type (enum-raw '( (0 null) (1 load) (2 dynamic) (3 interp)
                                (4 note) (6 phdr) )
                             'load))
       (cons 'flags flags)
       (cons 'offset offset)
       (cons 'vaddr vaddr)
       (cons 'paddr vaddr)
       (cons 'filesz filesz)
       (cons 'memsz memsz)
       (cons 'align align))))

  (define program-header-envs
    (if executable?
        (append
         (list (program-header-env "text" text-offset text-vaddr*
                                   text-size text-size))
         (if (or has-data? has-bss?)
             (list (program-header-env "data" data-offset data-vaddr
                                       data-size (+ data-size bss-size)))
             '()))
        '()))
  (define program-headers-bytes
    (apply bytes-append
           (map (lambda (env)
                  (serialize-struct rules 'program-header class endian env))
                program-header-envs)))

  ;; ---- 节头 ---------------------------------------------------------------
  (define (section-info name)
    (cond [(string=? name "null")
           (list 0 0 0 0 0 0 0)]
          [(string=? name "text")
           (list text-vaddr* text-offset text-size 0 0 (elf-section-align text))]
          [(string=? name "data")
           (list data-vaddr data-offset data-size 0 0 (elf-section-align data))]
          [(string=? name "bss")
           (list bss-vaddr bss-offset bss-size 0 0 (elf-section-align bss))]
          [(string=? name "shstrtab")
           (list 0 shstrtab-offset (bytes-length shstrtab-bytes) 0 0 1)]
          [(string=? name "strtab")
           (list 0 strtab-offset (bytes-length strtab-bytes) 0 0 1)]
          [(string=? name "symtab")
           (list 0 symtab-offset (bytes-length symtab-bytes)
                 (find-first (lambda (i) (string=? (list-ref section-order i) "strtab"))
                             (range section-count))
                 1 word)]
          [(string=? name "rela.text")
           (list 0 rela-offset (bytes-length relocation-bytes)
                 (find-first (lambda (i) (string=? (list-ref section-order i) "symtab"))
                             (range section-count))
                 (find-first (lambda (i) (string=? (list-ref section-order i) "text"))
                             (range section-count))
                 word)]
          [else (list 0 0 0 0 0 1)]))

  (define section-headers-bytes
    (apply bytes-append
           (map (lambda (name index)
                  (let* ([info (section-info name)]
                         [sh-type (cond [(string=? name "null") 'null]
                                        [(string=? name "bss") 'nobits]
                                        [(string=? name "shstrtab") 'strtab]
                                        [(string=? name "strtab") 'strtab]
                                        [(string=? name "symtab") 'symtab]
                                        [(string=? name "rela.text") 'rela]
                                        [else (section-rule-type rules name)])]
                         [sh-flags (flags-mask
                                    (cond [(string=? name "null") '()]
                                          [(string=? name "shstrtab") '()]
                                          [(string=? name "strtab") '()]
                                          [(string=? name "symtab") '()]
                                          [(string=? name "rela.text") '()]
                                          [else (section-rule-flags rules name)])
                                    shf-table)]
                         [env (list
                               (cons 'name (if (string=? name "null")
                                               0
                                               (alist-value
                                                (section-display-name name)
                                                shstrtab-offsets 0)))
                               (cons 'type sh-type)
                               (cons 'flags sh-flags)
                               (cons 'addr (first info))
                               (cons 'offset (second info))
                               (cons 'size (third info))
                               (cons 'link (fourth info))
                               (cons 'info (fifth info))
                               (cons 'addralign (sixth info))
                               (cons 'entsize
                                     (if (string=? name "symtab")
                                         (struct-size rules 'symbol class)
                                         (if (string=? name "rela.text")
                                             (struct-size rules 'relocation class)
                                             0))))])
                    (serialize-struct rules 'section-header class endian env)))
                section-order
                (range section-count))))

  ;; ---- 组装文件 -----------------------------------------------------------
  (define padding-size (- text-offset headers-end))
  (when (< padding-size 0)
    (error 'build-elf "text offset is before the end of headers"))

  (define elf-bytes
    (apply bytes-append
           (list header-bytes
                 program-headers-bytes
                 (make-bytes padding-size 0)
                 text-bytes
                 data-bytes
                 section-headers-bytes
                 shstrtab-bytes
                 strtab-bytes
                 symtab-bytes
                 relocation-bytes)))

  (define layout
    (list
     (cons 'entry entry-address)
     (cons 'phdrs program-header-envs)
     (cons 'shdrs section-order)
     (cons 'sections
           (map (lambda (ph) (cons (alist-value 'vaddr ph 0)
                                   (alist-value 'memsz ph 0)))
                program-header-envs))
     (cons 'size (bytes-length elf-bytes))))
  (list elf-bytes layout))

;; ---------------------------------------------------------------------------
;; 公开接口
;; ---------------------------------------------------------------------------

;; 生成 ELF 可执行文件 (ET_EXEC)。
(define (wrap-executable
         #:rules [rules (read-elf-rules)]
         #:class [class 32]
         #:endian [endian 'little]
         #:machine [machine 'riscv]
         #:text [text-bytes (bytes)]
         #:data [data-bytes (bytes)]
         #:bss-size [bss-size 0]
         #:symbols [symbols '()]
         #:load-addr [load-addr #x80000000]
         #:entry [entry '_start]
         #:align [align #x1000])
  (define sections
    (append
     (list (text-section text-bytes #:addr load-addr))
     (if (> (bytes-length data-bytes) 0)
         (list (data-section data-bytes
                             #:addr (+ load-addr
                                       (align-up (bytes-length text-bytes) 4))))
         '())
     (if (> bss-size 0)
         (list (bss-section bss-size
                            #:addr (+ load-addr
                                      (align-up (bytes-length text-bytes) 4)
                                      (bytes-length data-bytes))))
         '())))
  (first (build-elf #:rules rules
                    #:class class
                    #:endian endian
                    #:type 'exec
                    #:machine machine
                    #:sections sections
                    #:symbols symbols
                    #:load-addr load-addr
                    #:entry entry
                    #:align align)))

;; 生成 ELF 可重定位目标文件 (ET_REL),包含符号表与重定位项。
(define (wrap-object
         #:rules [rules (read-elf-rules)]
         #:class [class 32]
         #:endian [endian 'little]
         #:machine [machine 'riscv]
         #:text [text-bytes (bytes)]
         #:data [data-bytes (bytes)]
         #:bss-size [bss-size 0]
         #:symbols [symbols '()]
         #:relocations [relocations '()]
         #:align [align 4])
  (define sections
    (append
     (list (text-section text-bytes))
     (if (> (bytes-length data-bytes) 0)
         (list (data-section data-bytes))
         '())
     (if (> bss-size 0)
         (list (bss-section bss-size))
         '())))
  (first (build-elf #:rules rules
                    #:class class
                    #:endian endian
                    #:type 'rel
                    #:machine machine
                    #:sections sections
                    #:symbols symbols
                    #:relocations relocations
                    #:entry #f
                    #:align align)))

;; 把 ELF 字节写入文件(二进制模式),返回写入的字节串。
(define (write-elf-file bytes path)
  (with-output-to-file path #:exists 'replace #:mode 'binary
    (lambda ()
      (write-bytes bytes)
      (void)))
  bytes)

;; 从 ELF 字节中解析内存布局(entry/phdrs/shdrs/segments),对应规则中的
;; memory-layout 派生节。
(define (elf-memory-layout bytes #:class [class 32])
  (define word (if (= class 32) 4 8))
  (define (read-int offset size)
    (let loop ([i 0] [acc 0])
      (if (= i size)
          acc
          (loop (+ i 1)
                (+ acc (arithmetic-shift (bytes-ref bytes (+ offset i))
                                         (* 8 i)))))))
  ;; e_ident(16) + e_type(2) + e_machine(2) + e_version(4) = 24
  (define entry-offset 24)
  (define entry (read-int entry-offset word))
  (define phoff-offset (+ entry-offset word))
  (define shoff-offset (+ phoff-offset word))
  (define phoff (read-int phoff-offset word))
  (define shoff (read-int shoff-offset word))
  ;; e_flags(4) e_ehsize(2) e_phentsize(2) e_phnum(2) e_shentsize(2)
  (define phnum-offset (+ shoff-offset word 4 2 2))
  (define shnum-offset (+ phnum-offset 2 2))
  (define phnum (read-int phnum-offset 2))
  (define shnum (read-int shnum-offset 2))
  (list
   (cons 'entry entry)
   (cons 'phnum phnum)
   (cons 'shnum shnum)
   (cons 'phoff phoff)
   (cons 'shoff shoff)))
