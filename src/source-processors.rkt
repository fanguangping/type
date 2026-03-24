#lang racket

(provide (all-defined-out))

(require "translate-utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 根据给定的注释判断规则,递归移除 S-表达式中所有被判定为注释的子表达式。
;              规则由 rule 给出,但仅当 sexp 为列表时才应用(原子永远不会被移除)。
;              如果整个输入本身就是注释,则返回空列表;否则递归处理其子元素,
;              移除所有匹配规则的子列表。
; Input:
;   source -> 任意 S-表达式(原子、列表或嵌套列表)
;   rule   -> 单参数谓词函数,接受一个 S-表达式,返回 #t 表示该表达式是注释,应被移除。
;             注意:规则只对列表生效,原子会被自动忽略。
; Output: 移除了所有注释后的新 S-表达式
; Example:
;   (define (star-comment? x)
;     (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
;          (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
;   (comment-processor '(a (* comment *) b) star-comment?) => '(a b)
;   (comment-processor '((* start *) (x y) (* end *)) star-comment?) => '((x y))
;   (comment-processor '(*whole*) star-comment?) => '()                     ; 整个表达式被移除
;   (comment-processor 'a (lambda (x) #t)) => 'a                            ; 原子不受影响
;   (comment-processor '((a (* nested *) b)) star-comment?) => '((a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (comment-processor source rule)
  (define (not-list? t) (not (list? t)))
  (define (walk sexp stop-p leaf-p branch-p ctl-num)
    (if (= ctl-num 0)
        (error "failed")
        (if (stop-p sexp)
            (leaf-p sexp)
            (map (lambda (s) (walk s stop-p leaf-p branch-p (- ctl-num 1)))
                 (branch-p sexp)))))
  (define (comment? sexp)
    (and (list? sexp) (rule sexp)))
  (if (comment? source)
      '()   ; 整个输入是注释,整体移除
      (walk source
            not-list?
            identity
            (lambda (s) (filter (lambda (e) (not (comment? e))) s))
            5000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明:递归替换原子处理器
; 输入:
;   source 源S表达式
;   rule   替换规则
; 输出:
;   目标S表达式
; 例子:
;   (replace-processor
;    '(tag (func1 a b (func2 (func1 a b))))
;    '((func1 +) (func2 (*x))))
; =>
;    '(tag (+ a b ((*x) (+ a b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (replace-processor source rule)
  (log-print "[replace-processor]" source rule)
  (walk-replace source rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明:递归移除表达式处理器
; 输入:
;   source 源S表达式
;   rule   移除表达式   改为  移除条件
; 输出:
;   目标S表达式
; 例子:
;   (remove-processor
;    '(a () b (c ())) '(c ()))
; =>
;    '(a () b)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-processor source rule)
  (log-print "[remove-processor]" source rule)
  (define (remove source sexp)
    (cond
      [(equal? source sexp) '()]
      [(not (pair? source)) source]
      [else
       (let loop ([lst source])
         (cond
           [(null? lst) '()]
           [(equal? (car lst) sexp) (loop (cdr lst))]
           [(pair? (car lst))
            (cons (remove (car lst) sexp)
                  (loop (cdr lst)))]
           [else
            (cons (car lst) (loop (cdr lst)))]))]))
  (remove source rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明:递归升级表达式处理器
; 输入:
;   source 源S表达式
;   rule   升级列表表头标签
; 输出:
;   目标S表达式
; 例子:
;   (upgrade-processor '(:body a b) ':body)
; => '(a b)
;   (upgrade-processor '(a (:body b c) d) ':body)
; => '(a b c d)
;   (upgrade-processor '(a (:body (b (:body c))) d) ':body)
; => '(a (b c) d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (upgrade-processor source rule)
  (log-print "[upgrade-processor]" source rule)
  (define (flatmap f lst)
    (apply append (map f lst)))
  
  (define (walk-upgrade expr tag)
    (define (upgrade expr)
      (cond
        [(not (pair? expr)) (list expr)]
        [(equal? (car expr) tag)
         (apply append (flatmap upgrade (cdr expr)))]
        [else
         (let ([new-children (apply append (map upgrade expr))])
           (list new-children))]))
    (let ([result (upgrade expr)])
      (if (and (pair? result) (null? (cdr result)))
          (car result)
          result)))
  (walk-upgrade source rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 扫描 S-表达式,收集所有满足谓词 rule 的符号的路径
; Input:
;   source -> S-表达式
;   rule -> 谓词函数,接受符号并返回布尔值
; Output: 哈希表,键为符号,值为该符号出现位置的所有物理路径列表
; Example:
;   (scanner '(a (b c) a) (lambda (x) (eq? x 'a))) 
;   => 返回哈希表,其中 'a 对应 '((0) (2))(假设索引从0开始)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (scanner-processor source rule)
  (log-print "[scanner-processor]" source rule)
  (scanner source rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 对源程序进行重定位,将标签引用替换为带偏移的标记
;              该函数是 relocate-program 的简化包装,仅返回重定位后的新程序,
;              丢弃内部生成的标签映射表(如需映射表请直接使用 relocate-program)
; Input:
;   source -> 列表形式的程序,每个元素为指令或标签定义(具体格式由 rule 判断)
;   rule   -> 标签定义识别谓词,接受一个程序元素,返回 #t 表示该元素是标签定义
; Output: 重定位后的新程序,其中所有标签引用被替换为形如 "name:offset" 的符号
; Example:
;   (define prog '((label start) (beq x0 x1 start)))
;   (define (label-def? x) (and (pair? x) (eq? (car x) 'label)))
;   (relocate-processor prog label-def?) 
;   => '((label start) (beq x0 x1 start:0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (relocate-processor source rule)
  (log-print "[relocate-processor]" source rule)
  (define-values (new-program label-map) (relocate-program source rule))
  new-program)

(define (processor tag)
  (case tag
    ('comment
     comment-processor)
    ('replace
     replace-processor)
    ('remove
     remove-processor)
    ('upgrade
     upgrade-processor)
    ('scanner
     scanner-processor)
    ('relocate
     relocate-processor)
    (else
     #f)))



