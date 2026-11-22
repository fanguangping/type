#lang racket

;; ============================================================================
;; ELF 包装工序 (ELF wrap process)
;; ============================================================================
;; 构建链的末端工序。把构建环境中的代码、符号与重定位信息交给
;; src/wrapper/elf-file-wrapper.rkt,生成 ELF 可执行文件或可重定位目标文件。

(require "../wrapper/elf-file-wrapper.rkt")
(require "process-utils.rkt")

(provide elf-process)

;; 取列表第 i 项,越界时返回默认值。
(define (entry-ref entry index default)
  (if (> (length entry) index) (list-ref entry index) default))

;; 把 ((name value [bind] [type] [shndx] [size]) ...) 转换为符号结构。
(define (make-symbols entries)
  (map (lambda (entry)
         (make-elf-symbol
          (entry-ref entry 0 'unnamed)
          #:value (entry-ref entry 1 0)
          #:size (entry-ref entry 5 0)
          #:bind (entry-ref entry 2 'global)
          #:type (entry-ref entry 3 'func)
          #:shndx (entry-ref entry 4 'text)))
       entries))

;; 把 ((offset symbol [type] [addend]) ...) 转换为重定位结构。
(define (make-relocations entries)
  (map (lambda (entry)
         (make-relocation (entry-ref entry 0 0)
                          (entry-ref entry 1 "")
                          #:type (entry-ref entry 2 18)
                          #:addend (entry-ref entry 3 0)))
       entries))

(define (elf-process env options)
  (define kind (options-ref options 'kind 'executable))
  (unless (memq kind '(object executable))
    (error 'elf-process "unknown kind: ~a" kind))
  (define output
    (resolve-option-path (env-ref env 'base-dir) options 'output))
  (unless output
    (error 'elf-process "missing (output ...) option"))

  (define rules (read-elf-rules))
  (define class (options-ref options 'class 32))
  (define machine (options-ref options 'machine 'riscv))
  (define code (env-ref env 'code (bytes)))
  (define data (env-ref env 'data (bytes)))
  (define bss-size (env-ref env 'bss-size 0))
  (define symbols (make-symbols (env-ref env 'symbols '())))
  (define relocations (make-relocations (env-ref env 'relocs '())))

  (define elf-bytes
    (if (eq? kind 'object)
        (wrap-object #:rules rules
                     #:class class
                     #:machine machine
                     #:text code
                     #:data data
                     #:bss-size bss-size
                     #:symbols symbols
                     #:relocations relocations)
        (wrap-executable #:rules rules
                         #:class class
                         #:machine machine
                         #:text code
                         #:data data
                         #:bss-size bss-size
                         #:symbols symbols
                         #:load-addr (options-ref options 'load-addr #x80000000)
                         #:entry (options-ref options 'entry '_start))))

  (write-elf-file elf-bytes output)
  (env-set
   (env-update env 'artifacts (lambda (old) (append old (list output))) '())
   (string->symbol (format "elf-~a" kind))
   elf-bytes))
