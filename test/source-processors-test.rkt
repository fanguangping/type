#lang racket

(require rackunit
         rackunit/text-ui
         "../src/source-processors.rkt")

(define processor-tests
  (test-suite
   "处理器单元测试"

   ;; ============================================================================
   ;; comment-processor 测试(新增)
   ;; ============================================================================
   (test-suite
    "comment-processor"

    (test-case "基本移除顶层注释"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '(a (* comment *) b) star-comment?)
                             '(a b)))

    (test-case "嵌套注释"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '((* outer *) (a (* inner *) b) (* outer2 *)) star-comment?)
                             '((a b))))

    (test-case "非注释保持不变"
               (define (star-comment? x) #f)   ; 永不注释
               (check-equal? (comment-processor '(a b c) star-comment?) '(a b c)))

    (test-case "多个注释"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '((*c1*) x (*c2*) y (*c3*)) star-comment?)
                             '(x y)))

    (test-case "注释后剩余元素顺序"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '(a (*c1*) b (*c2*) c) star-comment?)
                             '(a b c)))

    (test-case "空列表"
               (define (star-comment? x) #t)   ; 永远返回 #t 但空列表不一定是注释,取决于规则
               (check-equal? (comment-processor '() star-comment?) '()))

    (test-case "整个表达式都是注释"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '(*whole*) star-comment?) '()))

    (test-case "注释部分在列表中间"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '((*c*) (x y) (*c*)) star-comment?) '((x y))))

    (test-case "深层嵌套中的注释"
               (define (star-comment? x)
                 (and (list? x) (symbol? (car x)) (string-prefix? (symbol->string (car x)) "*")
                      (symbol? (last x)) (string-suffix? (symbol->string (last x)) "*")))
               (check-equal? (comment-processor '(a (b (*c*) c) d) star-comment?) '(a (b c) d)))

    (test-case "自定义注释规则"
               (define (single-quote-comment? x)
                 (and (list? x) (eq? (car x) 'quote) (null? (cddr x))))  ; 将 (quote x) 视为注释
               (check-equal? (comment-processor '(a (quote b) c (quote d) e) single-quote-comment?)
                             '(a c e)))
    (test-case "注释规则返回 #t 但实际不是列表的情况(应忽略)"
               (define (bad-rule? x) #t)
               ;; 原子不会被传递给 comment?,因此原子不会被移除
               (check-equal? (comment-processor 'a bad-rule?) 'a)))

   ;; =========================================================================
   ;; replace-processor 测试
   ;; =========================================================================
   (test-suite
    "replace-processor"

    (test-case "基本替换"
               (check-equal? (replace-processor '(func1 a b) '((func1 +)))
                             '(+ a b)))

    (test-case "嵌套替换 (官方示例)"
               (check-equal? (replace-processor '(tag (func1 a b (func2 (func1 a b))))
                                                '((func1 +) (func2 (*x))))
                             '(tag (+ a b ((*x) (+ a b))))))

    (test-case "多个规则同时应用"
               (check-equal? (replace-processor '(f g (h i)) '((f a) (g b) (h c)))
                             '(a b (c i))))

    (test-case "无匹配规则时原样返回"
               (check-equal? (replace-processor '(a b c) '((d e))) '(a b c)))

    (test-case "规则中的新值为列表"
               (check-equal? (replace-processor '(x y) '((x (1 2)))) '((1 2) y))))

   ;; =========================================================================
   ;; remove-processor 测试
   ;; =========================================================================
   (test-suite
    "remove-processor"

    (test-case "基本移除顶层元素"
               (check-equal? (remove-processor '(a b c) 'b) '(a c)))

    (test-case "嵌套移除 (官方示例)"
               (check-equal? (remove-processor '(a () b (c ())) '(c ()))
                             '(a () b)))

    (test-case "移除子列表"
               (check-equal? (remove-processor '(a (b c) d) '(b c)) '(a d)))

    (test-case "移除不存在的元素"
               (check-equal? (remove-processor '(a b c) 'd) '(a b c)))

    (test-case "移除多个相同元素"
               (check-equal? (remove-processor '(a b a c a) 'a) '(b c)))

    (test-case "递归移除"
               (check-equal? (remove-processor '((a b) (a b) c) '(a b)) '(c)))

    (test-case "移除原子时不影响嵌套"
               (check-equal? (remove-processor '((a) b (c)) 'b) '((a) (c)))))

   ;; =========================================================================
   ;; upgrade-processor 测试
   ;; =========================================================================
   (test-suite
    "upgrade-processor"

    (test-case "单层升级 (官方示例)"
               (check-equal? (upgrade-processor '(:<= (a b)) ':<=) '(a b)))

    (test-case "嵌套升级 (官方示例)"
               (check-equal? (upgrade-processor '(a (:<= (b c)) d) ':<=) '(a b c d)))

    (test-case "深层嵌套 (官方示例)"
               (check-equal? (upgrade-processor '(a (:<= (b (:<= (c)))) d) ':<=)
                             '(a b c d)))

    (test-case "升级后结果为原子"
               (check-equal? (upgrade-processor '(:<= (a)) ':<=) 'a))

    (test-case "升级空列表"
               (check-equal? (upgrade-processor '(:<=) ':<=) '()))

    (test-case "无升级标签时原样返回"
               (check-equal? (upgrade-processor '(a b c) ':<=) '(a b c)))

    (test-case "多个升级标签"
               (check-equal? (upgrade-processor '((:<= (x y)) (:<= (p q))) ':<=)
                             '(x y p q)))

    (test-case "升级后保留其他结构"
               (check-equal? (upgrade-processor '(a (:<= ((b c) d)) e) ':<=)
                             '(a (b c) d e))))))

;; 运行所有测试
(run-tests processor-tests)

