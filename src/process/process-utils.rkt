#lang racket

;; ============================================================================
;; 工序公共工具 (process utilities)
;; ============================================================================
;; 构建环境与工序选项都是关联表,这里提供统一的存取函数。

(require "../rule-loader.rkt")

(provide env-ref
         env-set
         env-update
         options-ref
         resolve-option-path)

;; 在关联表中取值。
(define (env-ref env key [default #f])
  (let ([pair (assoc key env)])
    (if pair (cdr pair) default)))

;; 设置关联表中的值(存在则替换,否则追加到头部)。
(define (env-set env key value)
  (let ([pair (assoc key env)])
    (if pair
        (map (lambda (e) (if (eq? (car e) key) (cons key value) e)) env)
        (cons (cons key value) env))))

;; 用函数更新关联表中的值。
(define (env-update env key f [default #f])
  (env-set env key (f (env-ref env key default))))

;; 在工序选项中取值。
(define (options-ref opts key [default #f])
  (let ([pair (assoc key opts)])
    (if pair (cdr pair) default)))

;; 取选项中的路径,并相对 base-dir 解析。
(define (resolve-option-path base-dir opts key [default #f])
  (let ([value (options-ref opts key default)])
    (and value (relative->path-string base-dir value))))
