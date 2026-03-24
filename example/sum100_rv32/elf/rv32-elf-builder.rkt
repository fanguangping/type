#lang racket

(require "../../../src/translate-system.rkt")
(require "../../../src/translate-plugins.rkt")

(define (rv64elf src-dir build-dir src-file rule-files)
  (define (safe f t)
    (if (null? t) t
        (f t)))

  (define (not-list? t) (not (list? t)))

  (define (symbol-prefix? sym prefix)
    (string-prefix? (symbol->string sym) prefix))

  (define (symbol-suffix? sym prefix)
    (string-suffix? (symbol->string sym) prefix))

  (define (walk sexp stop-p leaf-p branch-p ctl-num)
    (if (= ctl-num 0)
        (error "failed")
        (if (stop-p sexp)
            (leaf-p sexp)
            (map (lambda (s) (walk s stop-p leaf-p branch-p (- ctl-num 1)))
                 (branch-p sexp)))))
  
  (define (remove-comments sexp)
    (define (comment? sexp)
      (and (list? sexp)
           (symbol? (safe first sexp))
           (symbol-prefix? (safe first sexp) "*")
           (symbol? (safe last sexp))
           (symbol-suffix? (safe last sexp) "*")))
    (define (walk-filter sexp)
      (walk sexp
            not-list?
            identity
            (lambda (s) (filter (lambda (e) (not (comment? e))) s))
            5000))
    (walk-filter sexp))
  
  (define (fullpath dir filename)
    (path->string (build-path dir filename)))

  (define (file->sexp filepath)
    (remove-comments
     (call-with-input-file filepath
       (lambda (in)
         (port->list read in)))))

  (define source
    (file->sexp (fullpath src-dir src-file)))
  (define rules-list
    (map (lambda (f)
           (file->sexp (fullpath src-dir f)))
         (reverse rule-files)))

  (define stage 0)
  (define output
    (foldr
     (lambda (rules src)
       (with-output-to-file (string-append build-dir "build-stage" (~s stage) ".txt") #:exists 'replace
         (lambda ()
           (pretty-display src)
           (void)))
       (set! stage (add1 stage))
       (translate (first src) (second src) rules))
     (list (car source) '()) rules-list))
  (first output))

;; [todo 优化] 采用elf wrapper包装
;; 将二进制字符串列表转换为 64 位 RISC‐V ELF 可执行文件(仅一个程序头 .text 段)
;; strings : 字符串列表,每个非空字符串应为 32 个 '0'/'1' 字符
;; elf-file : 输出文件名
;; 可选参数:
;;   #:load-addr   - .text 段加载的虚拟地址(64 位),默认 #x80000000
;;   #:align       - 页对齐大小,默认 #x1000
(define (bin-strings->elf strings elf-file
                          #:load-addr [load-addr #x80000000]
                          #:align [align #x1000])

  ;(displayln (degrade-processor strings ':<=))
  (displayln (string-append* strings))
  
  ;; 1. 过滤空串,将每个二进制字符串转换为 4 字节小端序字节(.text 段内容)
  (define code-bytes
    (apply bytes-append
           (for/list ([s (in-list strings)]
                      #:unless (string=? s ""))
             (define val (string->number s 2))
             ;; 转换为 4 字节小端无符号整数(指令本身总是 32 位)
             (integer->integer-bytes val 4 #f #f))))
  (define text-size (bytes-length code-bytes))

  ;; 2. 64 位 ELF 常量
  (define ELFCLASS64 2)                           ; 64 位
  (define ELFDATA2LSB 1)
  (define EV_CURRENT 1)
  (define EM_RISCV 243)
  (define ET_EXEC 2)
  (define PT_LOAD 1)
  (define PF_R 4)
  (define PF_X 1)
  (define PF_RX (bitwise-ior PF_R PF_X))

  ;; 节类型和标志
  (define SHT_PROGBITS 1)
  (define SHT_STRTAB 3)
  (define SHF_ALLOC 2)
  (define SHF_EXECINSTR 4)

  ;; 3. 构建节头字符串表 (.shstrtab) 内容
  ;; 节名称列表,第一个为空字符串(索引0),后面依次为各节名
  (define section-names '("" ".text" ".shstrtab"))
  ;; 构建字符串表:每个字符串以 null 结尾,整体以 null 开头
  (define shstrtab-bytes
    (apply bytes-append
           (for/list ([name section-names])
             (bytes-append (string->bytes/utf-8 name) #"\0"))))
  (define shstrtab-size (bytes-length shstrtab-bytes))
  ;; 计算每个节名在字符串表中的偏移(第一个空字符串偏移为 0)
  (define name-offsets
    (let loop ([names section-names] [pos 0] [acc '()])
      (if (null? names)
          (reverse acc)
          (let* ([name (car names)]
                 [len (string-length name)]
                 [next-pos (+ pos len 1)])
            (loop (cdr names) next-pos (cons pos acc))))))
  ;; 提取各节名偏移
  (define text-name-offset  (list-ref name-offsets 1))
  (define shstrtab-name-offset (list-ref name-offsets 2))

  ;; 4. 计算虚拟地址和文件偏移布局(仅 .text 段)
  (define text-vaddr load-addr)
  (define text-offset align)                       ; 文件偏移从页对齐开始

  ;; 5. 64 位 ELF 头大小、程序头大小、节头大小
  (define elf-header-size 64)
  (define program-header-size 56)
  (define section-header-size 64)
  (define program-header-count 1)                    ; 只有一个 LOAD 段
  (define program-header-table-size (* program-header-count program-header-size))
  (define section-header-count 3)                     ; 空节 + .text + .shstrtab
  (define section-header-table-size (* section-header-count section-header-size))

  ;; 6. 计算填充大小(从程序头表结束到 text-offset 之间的零填充)
  (define headers-size (+ elf-header-size program-header-table-size))
  (define padding-size (- text-offset headers-size))
  (when (< padding-size 0)
    (error "代码段偏移必须大于头总大小"))

  ;; 7. 构建 64 位 ELF 头 (64 字节)
  (define elf-header
    (bytes-append
     #"\x7F\x45\x4C\x46"                                   ; e_ident[0..3]
     (bytes ELFCLASS64 ELFDATA2LSB EV_CURRENT 0)           ; e_ident[4..7]
     (make-bytes 8 0)                                       ; e_ident[8..15] 填充
     (integer->integer-bytes ET_EXEC 2 #f #f)               ; e_type
     (integer->integer-bytes EM_RISCV 2 #f #f)              ; e_machine
     (integer->integer-bytes EV_CURRENT 4 #f #f)            ; e_version
     (integer->integer-bytes text-vaddr 8 #f #f)            ; e_entry (64 位)
     (integer->integer-bytes elf-header-size 8 #f #f)       ; e_phoff (程序头偏移)
     (integer->integer-bytes (+ text-offset text-size) 8 #f #f) ; e_shoff (节头表起始位置)
     (integer->integer-bytes 0 4 #f #f)                     ; e_flags
     (integer->integer-bytes elf-header-size 2 #f #f)       ; e_ehsize
     (integer->integer-bytes program-header-size 2 #f #f)   ; e_phentsize
     (integer->integer-bytes program-header-count 2 #f #f)  ; e_phnum
     (integer->integer-bytes section-header-size 2 #f #f)   ; e_shentsize
     (integer->integer-bytes section-header-count 2 #f #f)  ; e_shnum
     (integer->integer-bytes 2 2 #f #f)))                   ; e_shstrndx (指向 .shstrtab,索引为2)

  ;; 8. 构建唯一的程序头(.text 段,RX)
  (define program-header
    (bytes-append
     (integer->integer-bytes PT_LOAD 4 #f #f)               ; p_type
     (integer->integer-bytes PF_RX 4 #f #f)                 ; p_flags
     (integer->integer-bytes text-offset 8 #f #f)           ; p_offset
     (integer->integer-bytes text-vaddr 8 #f #f)            ; p_vaddr
     (integer->integer-bytes text-vaddr 8 #f #f)            ; p_paddr
     (integer->integer-bytes text-size 8 #f #f)             ; p_filesz
     (integer->integer-bytes text-size 8 #f #f)             ; p_memsz
     (integer->integer-bytes align 8 #f #f)))               ; p_align

  ;; 9. 构建三个节头(每个 64 字节)
  ;; 辅助函数:构建单个节头(64 位版本)
  (define (make-section-header name type flags addr offset size link info addralign entsize)
    (bytes-append
     (integer->integer-bytes name 4 #f #f)      ; sh_name
     (integer->integer-bytes type 4 #f #f)      ; sh_type
     (integer->integer-bytes flags 8 #f #f)     ; sh_flags (64 位)
     (integer->integer-bytes addr 8 #f #f)      ; sh_addr
     (integer->integer-bytes offset 8 #f #f)    ; sh_offset
     (integer->integer-bytes size 8 #f #f)      ; sh_size
     (integer->integer-bytes link 4 #f #f)      ; sh_link
     (integer->integer-bytes info 4 #f #f)      ; sh_info
     (integer->integer-bytes addralign 8 #f #f) ; sh_addralign
     (integer->integer-bytes entsize 8 #f #f))) ; sh_entsize

  ;; 空节(索引 0,全零)
  (define sh-null
    (make-section-header 0 0 0 0 0 0 0 0 0 0))

  ;; .text 节
  (define sh-text
    (make-section-header
     text-name-offset                     ; sh_name 指向 ".text"
     SHT_PROGBITS
     (bitwise-ior SHF_ALLOC SHF_EXECINSTR) ; flags = 0x6
     text-vaddr
     text-offset
     text-size
     0 0 1 0))

  ;; .shstrtab 节
  (define sh-shstrtab
    (make-section-header
     shstrtab-name-offset                  ; sh_name 指向 ".shstrtab"
     SHT_STRTAB
     0                                      ; flags = 0 (不分配内存)
     0                                      ; sh_addr = 0
     (+ text-offset text-size section-header-table-size) ; sh_offset (在节头表之后)
     shstrtab-size
     0 0 1 0))

  (define section-headers (bytes-append sh-null sh-text sh-shstrtab))

  ;; 10. 写入 ELF 文件
  (with-output-to-file elf-file #:exists 'replace
    (lambda ()
      (display elf-header)
      (display program-header)
      (display (make-bytes padding-size 0))   ; 填充到 text-offset
      (display code-bytes)                     ; .text 段内容
      (display section-headers)                 ; 节头表
      (display shstrtab-bytes)                  ; .shstrtab 内容
      (void))))




;; 将二进制字符串列表转换为 32 位 RISC‐V ELF 可执行文件(仅一个程序头 .text 段)
;; strings : 字符串列表,每个非空字符串应为 32 个 '0'/'1' 字符
;; elf-file : 输出文件名
;; 可选参数:
;;   #:load-addr   - .text 段加载的虚拟地址(32 位),默认 #x80000000
;;   #:align       - 页对齐大小,默认 #x1000
(define (bin-strings->elf/32 strings elf-file
                             #:load-addr [load-addr #x80000000]
                             #:align [align #x1000])
  
  (displayln (string-append* strings))
  
  ;; 1. 过滤空串,将每个二进制字符串转换为 4 字节小端序字节(.text 段内容)
  (define code-bytes
    (apply bytes-append
           (for/list ([s (in-list strings)]
                      #:unless (string=? s ""))
             (define val (string->number s 2))
             (if (equal? s "0110001100010101") ;先写死
                 (integer->integer-bytes val 2 #f #f)
                 ;; 转换为 4 字节小端无符号整数(指令本身总是 32 位)
                 (integer->integer-bytes val 4 #f #f))
             )))
  (define text-size (bytes-length code-bytes))

  ;; 2. 32 位 ELF 常量
  (define ELFCLASS32 1)                           ; 32 位
  (define ELFDATA2LSB 1)
  (define EV_CURRENT 1)
  (define EM_RISCV 243)
  (define ET_EXEC 2)
  (define PT_LOAD 1)
  (define PF_R 4)
  (define PF_X 1)
  (define PF_RX (bitwise-ior PF_R PF_X))

  ;; 节类型和标志
  (define SHT_PROGBITS 1)
  (define SHT_STRTAB 3)
  (define SHF_ALLOC 2)
  (define SHF_EXECINSTR 4)

  ;; 3. 构建节头字符串表 (.shstrtab) 内容 (与64位版本相同)
  (define section-names '("" ".text" ".shstrtab"))
  (define shstrtab-bytes
    (apply bytes-append
           (for/list ([name section-names])
             (bytes-append (string->bytes/utf-8 name) #"\0"))))
  (define shstrtab-size (bytes-length shstrtab-bytes))
  ;; 计算每个节名在字符串表中的偏移
  (define name-offsets
    (let loop ([names section-names] [pos 0] [acc '()])
      (if (null? names)
          (reverse acc)
          (let* ([name (car names)]
                 [len (string-length name)]
                 [next-pos (+ pos len 1)])
            (loop (cdr names) next-pos (cons pos acc))))))
  (define text-name-offset  (list-ref name-offsets 1))
  (define shstrtab-name-offset (list-ref name-offsets 2))

  ;; 4. 计算虚拟地址和文件偏移布局(仅 .text 段)
  (define text-vaddr load-addr)
  (define text-offset align)                       ; 文件偏移从页对齐开始

  ;; 5. 32 位 ELF 头大小、程序头大小、节头大小
  (define elf-header-size 52)                       ; 32位ELF头固定52字节
  (define program-header-size 32)                    ; 32位程序头32字节
  (define section-header-size 40)                     ; 32位节头40字节
  (define program-header-count 1)
  (define program-header-table-size (* program-header-count program-header-size))
  (define section-header-count 3)                     ; 空节 + .text + .shstrtab
  (define section-header-table-size (* section-header-count section-header-size))

  ;; 6. 计算填充大小
  (define headers-size (+ elf-header-size program-header-table-size))
  (define padding-size (- text-offset headers-size))
  (when (< padding-size 0)
    (error "代码段偏移必须大于头总大小"))

  ;; 7. 构建 32 位 ELF 头 (52 字节)
  (define elf-header
    (bytes-append
     #"\x7F\x45\x4C\x46"                                   ; e_ident[0..3]
     (bytes ELFCLASS32 ELFDATA2LSB EV_CURRENT 0)           ; e_ident[4..7]
     (make-bytes 8 0)                                       ; e_ident[8..15] 填充
     (integer->integer-bytes ET_EXEC 2 #f #f)               ; e_type
     (integer->integer-bytes EM_RISCV 2 #f #f)              ; e_machine
     (integer->integer-bytes EV_CURRENT 4 #f #f)            ; e_version
     (integer->integer-bytes text-vaddr 4 #f #f)            ; e_entry (32 位)
     (integer->integer-bytes elf-header-size 4 #f #f)       ; e_phoff (程序头偏移, 32 位)
     (integer->integer-bytes (+ text-offset text-size) 4 #f #f) ; e_shoff (节头表起始, 32 位)
     (integer->integer-bytes 0 4 #f #f)                     ; e_flags
     (integer->integer-bytes elf-header-size 2 #f #f)       ; e_ehsize
     (integer->integer-bytes program-header-size 2 #f #f)   ; e_phentsize
     (integer->integer-bytes program-header-count 2 #f #f)  ; e_phnum
     (integer->integer-bytes section-header-size 2 #f #f)   ; e_shentsize
     (integer->integer-bytes section-header-count 2 #f #f)  ; e_shnum
     (integer->integer-bytes 2 2 #f #f)))                   ; e_shstrndx (指向 .shstrtab,索引为2)

  ;; 8. 构建唯一的程序头(.text 段,RX) – 32位版本
  (define program-header
    (bytes-append
     (integer->integer-bytes PT_LOAD 4 #f #f)               ; p_type
     (integer->integer-bytes text-offset 4 #f #f)           ; p_offset
     (integer->integer-bytes text-vaddr 4 #f #f)            ; p_vaddr
     (integer->integer-bytes text-vaddr 4 #f #f)            ; p_paddr
     (integer->integer-bytes text-size 4 #f #f)             ; p_filesz
     (integer->integer-bytes text-size 4 #f #f)             ; p_memsz
     (integer->integer-bytes PF_RX 4 #f #f)                 ; p_flags
     (integer->integer-bytes align 4 #f #f)))               ; p_align

  ;; 9. 构建三个节头(每个 40 字节) – 32位版本
  ;; 辅助函数:构建单个节头(32 位)
  (define (make-section-header/32 name type flags addr offset size link info addralign entsize)
    (bytes-append
     (integer->integer-bytes name 4 #f #f)      ; sh_name
     (integer->integer-bytes type 4 #f #f)      ; sh_type
     (integer->integer-bytes flags 4 #f #f)     ; sh_flags (32 位)
     (integer->integer-bytes addr 4 #f #f)      ; sh_addr
     (integer->integer-bytes offset 4 #f #f)    ; sh_offset
     (integer->integer-bytes size 4 #f #f)      ; sh_size
     (integer->integer-bytes link 4 #f #f)      ; sh_link
     (integer->integer-bytes info 4 #f #f)      ; sh_info
     (integer->integer-bytes addralign 4 #f #f) ; sh_addralign
     (integer->integer-bytes entsize 4 #f #f))) ; sh_entsize

  ;; 空节(索引 0,全零)
  (define sh-null
    (make-section-header/32 0 0 0 0 0 0 0 0 0 0))

  ;; .text 节
  (define sh-text
    (make-section-header/32
     text-name-offset                     ; sh_name 指向 ".text"
     SHT_PROGBITS
     (bitwise-ior SHF_ALLOC SHF_EXECINSTR) ; flags = 0x6
     text-vaddr
     text-offset
     text-size
     0 0 1 0))

  ;; .shstrtab 节
  (define sh-shstrtab
    (make-section-header/32
     shstrtab-name-offset                  ; sh_name 指向 ".shstrtab"
     SHT_STRTAB
     0                                      ; flags = 0 (不分配内存)
     0                                      ; sh_addr = 0
     (+ text-offset text-size section-header-table-size) ; sh_offset (在节头表之后)
     shstrtab-size
     0 0 1 0))

  (define section-headers (bytes-append sh-null sh-text sh-shstrtab))

  ;; 10. 写入 ELF 文件
  (with-output-to-file elf-file #:exists 'replace
    (lambda ()
      (display elf-header)
      (display program-header)
      (display (make-bytes padding-size 0))   ; 填充到 text-offset
      (display code-bytes)                     ; .text 段内容
      (display section-headers)                 ; 节头表
      (display shstrtab-bytes)                  ; .shstrtab 内容
      (void))))

(define output
  (rv64elf "../"
           "./build/"
           "example.scm"
           '("rules-0.scm" "rules-1.scm" "elf/rules-2.scm"  "elf/rules-link.scm"  "elf/rules-bin.scm")))

;(pretty-display output)

(bin-strings->elf/32 (second output) "sum100.elf")



