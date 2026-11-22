#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         racket/runtime-path
         "../src/wrapper/elf-file-wrapper.rkt")

(define-runtime-path rules-path
  (build-path ".." "ruleset" "elf" "elf-file.scm"))

;; 在关联表中查找 key 的值。
(define (alist-value key alst [default #f])
  (let ([pair (assoc key alst)])
    (if pair (cdr pair) default)))

;; ============================================================================
;; 测试辅助
;; ============================================================================

;; 从字节串的 offset 处读取 size 字节的小端无符号整数。
(define (read-le bytes offset size)
  (let loop ([i 0] [acc 0])
    (if (= i size)
        acc
        (loop (+ i 1)
              (+ acc (arithmetic-shift (bytes-ref bytes (+ offset i))
                                       (* 8 i)))))))

(define test-rules (read-elf-rules rules-path))

(define test-code
  (code-strings->bytes
   (list "00000000000000010000000100010011"
         "00000000000000000000000001110011")))

(define test-symbols
  (list (make-elf-symbol "_start" #:value 0 #:size 8
                         #:bind 'global #:type 'func #:shndx 'text)
        (make-elf-symbol "putchar" #:value 0 #:size 0
                         #:bind 'global #:type 'notype #:shndx 'undef)))

;; 在字节串中查找 ASCII 子串,找到返回 #t,找不到返回 #f。
(define (find-bytes needle haystack)
  (let loop ([i 0])
    (cond [(> (+ i (bytes-length needle)) (bytes-length haystack)) #f]
          [(bytes=? (subbytes haystack i (+ i (bytes-length needle))) needle) #t]
          [else (loop (+ i 1))])))

;; 在 ELF32 文件中按节名查找节头,返回该节头的各字段(关联表)。
(define (find-section elf name)
  (define shoff (read-le elf 32 4))
  (define shentsize (read-le elf 46 2))
  (define shnum (read-le elf 48 2))
  (define shstrndx (read-le elf 50 2))
  (define strtab-offset
    (+ shoff (* shstrndx shentsize)))
  (define strtab-start (read-le elf (+ strtab-offset 16) 4))
  (define strtab-size (read-le elf (+ strtab-offset 20) 4))
  (define names (subbytes elf strtab-start (+ strtab-start strtab-size)))
  (let loop ([i 0])
    (if (>= i shnum)
        #f
        (let* ([base (+ shoff (* i shentsize))]
               [name-offset (read-le elf base 4)]
               [end (let scan ([j name-offset])
                      (if (or (>= j (bytes-length names))
                              (= (bytes-ref names j) 0))
                          j
                          (scan (+ j 1))))]
               [section-name (bytes->string/utf-8
                              (subbytes names name-offset end))])
          (if (string=? section-name name)
              (list (cons 'type (read-le elf (+ base 4) 4))
                    (cons 'addr (read-le elf (+ base 12) 4))
                    (cons 'offset (read-le elf (+ base 16) 4))
                    (cons 'size (read-le elf (+ base 20) 4)))
              (loop (+ i 1)))))))

;; ============================================================================
;; 规则解析测试
;; ============================================================================

(define rule-tests
  (test-suite
   "ELF rule tests"

   (test-case "read-elf-rules 读取全部规则节"
     (check-true (list? test-rules))
     (check-true (pair? test-rules))
     (for-each (lambda (name)
                 (check-true (pair? (rules-ref test-rules name))
                             (format "missing rule section: ~a" name)))
               '(fileinfo header program-header section-header
                          symbol relocation string-table segments
                          sections memory-layout)))

   (test-case "struct-size 计算 32/64 位结构大小"
     (check-equal? (struct-size test-rules 'header 32) 52)
     (check-equal? (struct-size test-rules 'header 64) 64)
     (check-equal? (struct-size test-rules 'program-header 32) 32)
     (check-equal? (struct-size test-rules 'program-header 64) 56)
     (check-equal? (struct-size test-rules 'section-header 32) 40)
     (check-equal? (struct-size test-rules 'section-header 64) 64)
     (check-equal? (struct-size test-rules 'symbol 32) 16)
     (check-equal? (struct-size test-rules 'symbol 64) 24)
     (check-equal? (struct-size test-rules 'relocation 32) 12)
     (check-equal? (struct-size test-rules 'relocation 64) 24))))

;; ============================================================================
;; 基础工具测试
;; ============================================================================

(define util-tests
  (test-suite
   "ELF wrapper utility tests"

   (test-case "code-strings->bytes 将 32 位二进制字符串转为小端字节"
     (check-equal? (code-strings->bytes '()) (bytes))
     (check-equal? (code-strings->bytes
                    (list "00000000000000000000000000000001"
                          "00000000000000000000000000000010"))
                   (bytes 1 0 0 0 2 0 0 0))
     (check-equal? (code-strings->bytes (list "" "not-binary"))
                   (bytes)))

   (test-case "make-string-table 生成 NUL 分隔表并计算偏移"
     (define table (make-string-table '("a" "bc")))
     (check-equal? (car table) #"\0a\0bc\0")
     (check-equal? (alist-value "a" (cdr table)) 1)
     (check-equal? (alist-value "bc" (cdr table)) 3))

   (test-case "symbol-info 组合 binding 与 type"
     (check-equal? (symbol-info
                    (make-elf-symbol "x" #:bind 'global #:type 'func))
                   #x12)
     (check-equal? (symbol-info
                    (make-elf-symbol "x" #:bind 'local #:type 'notype))
                   0))))

;; ============================================================================
;; 可执行文件测试 (issue #8)
;; ============================================================================

(define executable-tests
  (test-suite
   "ELF executable tests"

   (test-case "wrap-executable 生成合法的 ELF32 头"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:symbols test-symbols
                                  #:entry '_start))
     (check-equal? (subbytes elf 0 4) (bytes #x7f #x45 #x4c #x46))
     (check-equal? (bytes-ref elf 4) 1)
     (check-equal? (bytes-ref elf 5) 1)
     (check-equal? (read-le elf 16 2) 2)
     (check-equal? (read-le elf 18 2) 243)
     (check-equal? (read-le elf 24 4) #x80000000)
     (check-equal? (read-le elf 28 4) 52)
     (check-equal? (read-le elf 40 2) 52)
     (check-equal? (read-le elf 42 2) 32)
     (check-equal? (read-le elf 44 2) 1)
     (check-equal? (read-le elf 46 2) 40)
     (check-equal? (read-le elf 48 2) 5)
     (check-equal? (read-le elf 50 2) 2))

   (test-case "wrap-executable 生成合法的 ELF64 头"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 64
                                  #:text test-code
                                  #:symbols test-symbols
                                  #:entry '_start))
     (check-equal? (bytes-ref elf 4) 2)
     (check-equal? (read-le elf 16 2) 2)
     (check-equal? (read-le elf 24 8) #x80000000)
     (check-equal? (read-le elf 32 8) 64)
     (check-equal? (read-le elf 52 2) 64)
     (check-equal? (read-le elf 54 2) 56)
     (check-equal? (read-le elf 58 2) 64))

   (test-case "入口点支持按符号名解析"
     (define symbols
       (list (make-elf-symbol "real-start" #:value #x100
                              #:bind 'global #:type 'func #:shndx 'text)))
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:symbols symbols
                                  #:entry 'real-start))
     (check-equal? (read-le elf 24 4) (+ #x80000000 #x100)))

   (test-case "入口点可指定为数字地址"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:entry #x80001000))
     (check-equal? (read-le elf 24 4) #x80001000))

   (test-case "包含 .text/.data/.bss 节与两个程序头"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:data (bytes 1 2 3 4)
                                  #:bss-size 16
                                  #:symbols test-symbols
                                  #:entry '_start))
     (check-equal? (read-le elf 44 2) 2)
     (check-true (find-bytes #".text" elf))
     (check-true (find-bytes #".data" elf))
     (check-true (find-bytes #".bss" elf))
     (check-true (find-bytes #".shstrtab" elf))
     (check-true (find-bytes #".symtab" elf)))

   (test-case "符号写入符号表"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:symbols test-symbols
                                  #:entry '_start))
     (check-true (find-bytes #"_start" elf))
     (check-true (find-bytes #"putchar" elf)))

   (test-case "大端序写入 e_ident"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:endian 'big
                                  #:text test-code
                                  #:entry '_start))
     (check-equal? (bytes-ref elf 5) 2))))

;; ============================================================================
;; 目标文件测试 (issue #9)
;; ============================================================================

(define object-tests
  (test-suite
   "ELF object file tests"

   (test-case "wrap-object 生成 ET_REL 且无程序头"
     (define obj (wrap-object #:rules test-rules
                              #:class 32
                              #:text test-code
                              #:symbols test-symbols))
     (check-equal? (read-le obj 16 2) 1)
     (check-equal? (read-le obj 24 4) 0)
     (check-equal? (read-le obj 28 4) 0)
     (check-equal? (read-le obj 44 2) 0))

   (test-case "目标文件包含符号表与重定位节"
     (define obj (wrap-object #:rules test-rules
                              #:class 32
                              #:text test-code
                              #:symbols test-symbols
                              #:relocations
                              (list (make-relocation 0 "_start"
                                                     #:type 18 #:addend 0))))
     (check-true (find-bytes #".symtab" obj))
     (check-true (find-bytes #".rela.text" obj))
     (check-true (find-bytes #"_start" obj))
     (check-equal? (read-le obj 48 2) 6))

   (test-case "重定位项计入节大小"
     (define no-reloc
       (wrap-object #:rules test-rules #:class 32 #:text test-code
                    #:symbols test-symbols))
     (define with-reloc
       (wrap-object #:rules test-rules #:class 32 #:text test-code
                    #:symbols test-symbols
                    #:relocations
                    (list (make-relocation 0 "_start"
                                           #:type 18 #:addend 0))))
     (define rela (find-section with-reloc ".rela.text"))
     (check-true (pair? rela))
     ;; 每条 ELF32 Rela 重定位项为 12 字节,这里只有一条。
     (check-equal? (alist-value 'size rela) 12)
     (check-false (find-section no-reloc ".rela.text")))

   (test-case "目标文件支持 ELF64"
     (define obj (wrap-object #:rules test-rules
                              #:class 64
                              #:text test-code
                              #:symbols test-symbols
                              #:relocations
                              (list (make-relocation 4 "putchar" #:type 18))))
     (check-equal? (bytes-ref obj 4) 2)
     (check-equal? (read-le obj 16 2) 1)
     (check-true (find-bytes #".rela.text" obj)))))

;; ============================================================================
;; 文件写入与布局测试
;; ============================================================================

(define io-tests
  (test-suite
   "ELF file IO and layout tests"

   (test-case "write-elf-file 写入完整字节"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:entry '_start))
     (define path (build-path (find-system-path 'temp-dir)
                              "type-elf-test.elf"))
     (write-elf-file elf path)
     (check-equal? (file->bytes path) elf)
     (delete-file path))

   (test-case "elf-memory-layout 解析入口与头位置"
     (define elf (wrap-executable #:rules test-rules
                                  #:class 32
                                  #:text test-code
                                  #:entry '_start))
     (define layout (elf-memory-layout elf #:class 32))
     (check-equal? (alist-value 'entry layout) #x80000000)
     (check-equal? (alist-value 'phoff layout) 52)
     (check-equal? (alist-value 'phnum layout) 1)
     (check-equal? (alist-value 'shnum layout) 5))))

(run-tests (test-suite "ELF file wrapper tests"
                       rule-tests
                       util-tests
                       executable-tests
                       object-tests
                       io-tests))
