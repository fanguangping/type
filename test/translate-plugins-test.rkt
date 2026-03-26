#lang racket

(require rackunit
         rackunit/text-ui
         "../src/translate-plugins.rkt")

;; 通过 plugin 获取各处理器
(define eval-handler (plugin ':=))
(define macro-expand-handler (plugin '>=<))
(define local-bind-handler (plugin ':))
(define local-append-handler (plugin ':+))
(define global-bind-handler (plugin '!))
(define global-append-handler (plugin '!+))
(define global-init-handler (plugin '!!))
(define origin-handler (plugin ':::))

;; ============================================================================
;; 测试辅助数据
;; ============================================================================

;; 局部绑定
(define local-base
  '((a 1) (b 2) (c 3) (d ("hello" "world"))))

;; 全局绑定(包含一个原始代码条目用于 origin-handler)
(define global-base
  '((x 1) (y 2) (z 3) (w ("rv" 64)) (::: "this is original code: (\"hello\" \"world\")")))

;; 规则选择器,模仿原参考代码的格式
(define (rule-selector sym)
  (case sym
    [(add-one)
     ;; 返回 (function lambda (x) (+ x 1)),cdr 部分为 (lambda (x) (+ x 1)),可被 eval 为过程
     (cons 'function (cons 'lambda (cdr '(f (x) (+ x 1)))))]
    [(f)
     ;; 宏定义
     '(macro f (a b c) (x (a 1) (b 5) (c 8)))]
    [(g)
     '(macro g (x y z) (a (x 5) (y d) (z 3)))]
    [else '()]))

;; ============================================================================
;; 测试套件
;; ============================================================================

(define plugin-tests
  (test-suite
   "插件处理器单元测试"

   ;; ------------------------------------------------------------------------
   ;; eval-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "eval-handler"

    (test-case "基本算术运算,使用局部变量"
               (define result (eval-handler '(+ a b c) rule-selector local-base global-base))
               (check-equal? (first result) 6)                ; 1+2+3
               (check-equal? (second result) local-base)      ; local 不变
               (check-equal? (third result) global-base))     ; global 不变

    (test-case "嵌套算术表达式"
               (define result (eval-handler '(+ a (* b c)) rule-selector local-base global-base))
               (check-equal? (first result) 7)                ; 1 + (2*3)
               (check-equal? (second result) local-base)
               (check-equal? (third result) global-base))

    (test-case "使用内置函数 map 和自定义函数 add-one"
               (define result (eval-handler '(map add-one (list a b c)) rule-selector local-base global-base))
               (check-equal? (first result) '(2 3 4))         ; (1+1 2+1 3+1)
               (check-equal? (second result) local-base)
               (check-equal? (third result) global-base))

    (test-case "列表元素求值"
               (define result (eval-handler '(a b c) rule-selector local-base global-base))
               (check-equal? (first result) '(1 2 3))
               (check-equal? (second result) local-base)
               (check-equal? (third result) global-base))

    (test-case "未绑定的符号原样返回"
               (define result (eval-handler '(unknown-sym) rule-selector local-base global-base))
               (check-equal? (first result) '(unknown-sym))
               (check-equal? (second result) local-base)
               (check-equal? (third result) global-base)))

   ;; ============================================================================
   ;; macro-expand-handler 测试
   ;; ============================================================================
   (test-suite
    "macro-expand-handler"

    (test-case "基本匹配 - 三个参数"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (a b c) (+ a b c))] [else #f]))
               (define node '(f 1 2 3))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((+ 1 2 3))))
               (check-equal? new-local '((a 1) (b 2) (c 3)))
               (check-equal? global '()))

    (test-case "剩余参数打包 - 多个剩余参数"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (a b rest) (list a b rest))] [else #f]))
               (define node '(f 1 2 3 4))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((list 1 2 (3 4)))))
               (check-equal? new-local '((a 1) (b 2) (rest (3 4)))))

    (test-case "剩余参数打包 - 没有剩余参数"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (a b rest) (list a b rest))] [else #f]))
               (define node '(f 1 2))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((list 1 2 ()))))
               (check-equal? new-local '((a 1) (b 2) (rest ()))))

    (test-case "带可选关键字 - 全部提供"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (func-name #:in in-regs #:out out-regs body) (some-op body))] [else #f]))
               (define node '(f main (in (int start) (int end)) (out (int result)) (op1 op2)))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((some-op (op1 op2)))))
               (check-equal? new-local '((func-name main) (in-regs (in (int start) (int end))) (out-regs (out (int result))) (body (op1 op2)))))

    (test-case "带可选关键字 - 只提供out和body"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (func-name #:in in-regs #:out out-regs body) (some-op body))] [else #f]))
               (define node '(f main (out (int result)) (op1 op2)))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((some-op (op1 op2)))))
               (check-equal? new-local '((func-name main) (in-regs ()) (out-regs (out (int result))) (body (op1 op2)))))

    (test-case "带可选关键字 - 只提供in和body"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (func-name #:in in-regs #:out out-regs body) (some-op body))] [else #f]))
               (define node '(f main (in (int start)) (op1 op2)))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((some-op (op1 op2)))))
               (check-equal? new-local '((func-name main) (in-regs (in (int start))) (out-regs ()) (body (op1 op2)))))

    (test-case "带可选关键字 - 只提供body"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (func-name #:in in-regs #:out out-regs body) (some-op body))] [else #f]))
               (define node '(f main (op1 op2)))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((some-op (op1 op2)))))
               (check-equal? new-local '((func-name main) (in-regs ()) (out-regs ()) (body (op1 op2)))))

    (test-case "多个可选关键字 - args和body"
               (define (rule-selector sym)
                 (case sym [(p) '(macro p (#:args args body) (apply-op args body))] [else #f]))
               (define node '(p (args (a b c)) (func-body)))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((apply-op (args (a b c)) (func-body)))))
               (check-equal? new-local '((args (args (a b c))) (body (func-body)))))

    (test-case "多个可选关键字 - 只提供body"
               (define (rule-selector sym)
                 (case sym [(p) '(macro p (#:args args body) (apply-op args body))] [else #f]))
               (define node '(p (func-body)))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((apply-op () (func-body)))))
               (check-equal? new-local '((args ()) (body (func-body)))))

    (test-case "嵌套结构中的变量替换"
               (define (rule-selector sym)
                 (case sym [(f) '(macro f (a b) (list a (list b)))] [else #f]))
               (define node '(f 1 2))
               (define result (macro-expand-handler node rule-selector '() '()))
               (define new-code (first result))
               (define new-local (second result))
               (define global (third result))
               (check-equal? new-code '(:<= ((list 1 (list 2)))))
               (check-equal? new-local '((a 1) (b 2))))

    (test-case "无效宏名错误"
               (define (rule-selector sym) #f)
               (define node '(unknown 1 2))
               (check-exn exn:fail? (lambda () (macro-expand-handler node rule-selector '() '())))))

   ;; ------------------------------------------------------------------------
   ;; local-bind-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "local-bind-handler"

    (test-case "绑定新变量"
               (define result (local-bind-handler '(: e (+ a b c)) rule-selector local-base global-base))
               (define expected-local '((a 1) (b 2) (c 3) (d ("hello" "world")) (e 6)))
               (check-equal? (first result) '())          ; 无代码输出
               (check-equal? (second result) expected-local)
               (check-equal? (third result) global-base))

    (test-case "更新已有变量"
               (define result (local-bind-handler '(: a (* b c)) rule-selector local-base global-base))
               (define expected-local '((a 6) (b 2) (c 3) (d ("hello" "world"))))
               (check-equal? (first result) '())
               (check-equal? (second result) expected-local)
               (check-equal? (third result) global-base)))

   ;; ------------------------------------------------------------------------
   ;; local-append-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "local-append-handler"

    (test-case "向列表变量追加值"
               ;; 初始 d 是 ("hello" "world")
               (define result (local-append-handler '(:+ d "new") rule-selector local-base global-base))
               (define expected-local '((a 1) (b 2) (c 3) (d ("hello" "world" "new"))))
               (check-equal? (first result) '())
               (check-equal? (second result) expected-local)
               (check-equal? (third result) global-base))

    (test-case "向原子变量追加(首次追加)"
               ;; a 原为 1,追加后应变为 (1 新值)
               (define result (local-append-handler '(:+ a 100) rule-selector local-base global-base))
               (define expected-local '((a (1 100)) (b 2) (c 3) (d ("hello" "world"))))
               (check-equal? (first result) '())
               (check-equal? (second result) expected-local)
               (check-equal? (third result) global-base))

    (test-case "向不存在的变量追加(创建新列表)"
               (define result (local-append-handler '(:+ new 42) rule-selector local-base global-base))
               (define expected-local (append local-base '((new (42)))))
               (check-equal? (first result) '())
               (check-equal? (second result) expected-local)
               (check-equal? (third result) global-base)))

   ;; ------------------------------------------------------------------------
   ;; global-init-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "global-init-handler"

    (test-case "初始化多个全局变量"
               (define nodes '((p (+ 1 2)) (q (* 3 4))))
               (define result (global-init-handler nodes rule-selector '() '()))
               (define expected-global '((p 3) (q 12)))
               (check-equal? (first result) expected-global)
               (check-equal? (second result) '())
               (check-equal? (third result) expected-global))

    (test-case "使用局部变量初始化全局变量"
               (define nodes '((r (+ a b))))
               (define result (global-init-handler nodes rule-selector local-base '()))
               (define expected-global '((r 3)))
               (check-equal? (first result) expected-global)
               (check-equal? (second result) local-base)
               (check-equal? (third result) expected-global)))

   ;; ------------------------------------------------------------------------
   ;; global-bind-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "global-bind-handler"

    (test-case "绑定新全局变量"
               (define result (global-bind-handler '(! new (* 2 5)) rule-selector local-base global-base))
               (define expected-global (append global-base '((new 10))))
               (check-equal? (first result) '())
               (check-equal? (second result) local-base)
               (check-equal? (third result) expected-global))

    (test-case "更新已有全局变量"
               (define result (global-bind-handler '(! x (+ a b c)) rule-selector local-base global-base))
               (define expected-global '((x 6) (y 2) (z 3) (w ("rv" 64)) (::: "this is original code: (\"hello\" \"world\")")))
               (check-equal? (first result) '())
               (check-equal? (second result) local-base)
               (check-equal? (third result) expected-global)))

   ;; ------------------------------------------------------------------------
   ;; global-append-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "global-append-handler"

    (test-case "向全局列表变量追加值"
               ;; w 原为 ("rv" 64)
               (define result (global-append-handler '(!+ w "new") rule-selector local-base global-base))
               (define expected-global '((x 1) (y 2) (z 3) (w ("rv" 64 "new")) (::: "this is original code: (\"hello\" \"world\")")))
               (check-equal? (first result) '())
               (check-equal? (second result) local-base)
               (check-equal? (third result) expected-global))

    (test-case "向原子全局变量追加"
               ;; y 原为 2
               (define result (global-append-handler '(!+ y 100) rule-selector local-base global-base))
               (define expected-global '((x 1) (y (2 100)) (z 3) (w ("rv" 64)) (::: "this is original code: (\"hello\" \"world\")")))
               (check-equal? (first result) '())
               (check-equal? (second result) local-base)
               (check-equal? (third result) expected-global)))

   ;; ------------------------------------------------------------------------
   ;; origin-handler 测试
   ;; ------------------------------------------------------------------------
   (test-suite
    "origin-handler"

    (test-case "获取原始代码"
               ;; 注意:节点为 '(:::),全局中应存在对应的 '::: 条目
               (define result (origin-handler '(:::) rule-selector local-base global-base))
               (check-equal? (first result) "this is original code: (\"hello\" \"world\")")
               (check-equal? (second result) local-base)
               (check-equal? (third result) global-base))

    (test-case "不存在的原始代码返回空列表"
               (define result (origin-handler '(nonexistent) rule-selector local-base global-base))
               ;; 由于 (car node) 是 'nonexistent,全局中没有,返回 '()
               (check-equal? (first result) '())
               (check-equal? (second result) local-base)
               (check-equal? (third result) global-base)))))

;; ============================================================================
;; 运行测试
;; ============================================================================
(run-tests plugin-tests)


