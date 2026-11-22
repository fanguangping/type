#lang racket

;; ============================================================================
;; 编码工序 (encode process)
;; ============================================================================
;; 把 32 位二进制字符串列表转换为字节串,为包装工序准备 'code。

(require "../wrapper/elf-file-wrapper.rkt")
(require "process-utils.rkt")
(require "symbol-tools.rkt")

(provide encode-process)

(define (encode-process env options)
  (define from-key (options-ref options 'from 'source))
  (define to-key (options-ref options 'to 'code))
  (define value (env-ref env from-key '()))
  ;; 若来源是 (:program (code...) ...) 程序形式,取其中的代码段。
  (define code-strings
    (if (and (pair? value) (eq? (car value) ':program))
        (program-code value)
        value))
  (env-set env to-key (code-strings->bytes code-strings)))
