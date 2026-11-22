#lang racket

;; ============================================================================
;; 符号与重定位提取工具 (symbol tools)
;; ============================================================================
;; 从链接后的 S 表达式程序中提取标签地址与 call 重定位信息。
;; 这些结果是「链接工序」的产物,交给后续「包装工序」序列化,
;; 使包装器无需反向解析源码。

(require racket/string)

(provide strip-offset
         instruction-form?
         program-code
         collect-labels
         collect-relocations)

;; 去掉 "label:offset" 中的 ":offset" 后缀。
(define (strip-offset sym)
  (if (symbol? sym)
      (string->symbol (first (string-split (symbol->string sym) ":")))
      sym))

;; label/global/note 不占用指令槽。
(define (instruction-form? form)
  (not (and (list? form) (pair? form)
            (memq (first form) '(label global note)))))

;; 取程序中的代码形式列表。程序形如 (:program (code...) (:data ...))。
(define (program-code program)
  (if (and (list? program) (>= (length program) 2))
      (second program)
      '()))

;; 收集标签偏移,每条指令 4 字节。
;; 返回 ((name value) ...),name 为符号,value 为字节偏移。
(define (collect-labels program)
  (let loop ([forms (program-code program)] [index 0] [acc '()])
    (cond [(null? forms) (reverse acc)]
          [(and (list? (car forms)) (pair? (car forms))
                (eq? (first (car forms)) 'label))
           (loop (cdr forms) index
                 (cons (list (strip-offset (second (car forms))) (* index 4))
                       acc))]
          [(instruction-form? (car forms))
           (loop (cdr forms) (+ index 1) acc)]
          [else (loop (cdr forms) index acc)])))

;; 收集 call 指令的重定位项。R_RISCV_CALL = 18。
;; 返回 ((offset symbol type addend) ...)。
(define (collect-relocations program #:type [type 18] #:addend [addend 0])
  (let loop ([forms (program-code program)] [index 0] [acc '()])
    (cond [(null? forms) (reverse acc)]
          [(and (list? (car forms)) (pair? (car forms))
                (eq? (first (car forms)) 'call))
           (loop (cdr forms) (+ index 1)
                 (cons (list (* index 4)
                             (strip-offset (second (car forms)))
                             type addend)
                       acc))]
          [(instruction-form? (car forms))
           (loop (cdr forms) (+ index 1) acc)]
          [else (loop (cdr forms) index acc)])))
