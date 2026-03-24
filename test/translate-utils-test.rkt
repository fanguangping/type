#lang racket

(require rackunit
         rackunit/text-ui
         "../src/translate-utils.rkt")

;; 将所有测试组织在一个 test-suite 中,最后运行
(define all-tests
  (test-suite
   "translate-utils 单元测试"

   ;; =========================================================================
   ;; 辅助函数
   ;; =========================================================================
   (test-suite
    "辅助函数"

    (test-case "atom?"
      (check-true (atom? 'a))
      (check-true (atom? 42))
      (check-true (atom? "hello"))
      (check-false (atom? '(a b)))
      (check-false (atom? '())))

    (test-case "safe"
      (check-equal? (safe car '(a b c)) 'a)
      (check-equal? (safe car '()) '()))

    (test-case "tag?"
      (check-true (tag? '(in x) 'in))
      (check-false (tag? '(out y) 'in))
      (check-false (tag? '() 'in)))

    (test-case "sym-seqs"
      (check-equal? (sym-seqs 'x 3) '(x0 x1 x2))
      (check-equal? (sym-seqs 'reg 0) '()))

    (test-case "flatmap"
      (check-equal? (flatmap (lambda (x) (list x x)) '(1 2 3))
                    '(1 1 2 2 3 3))
      (check-equal? (flatmap list '()) '()))

    (test-case "flatmap2"
      (check-equal? (flatmap2 list '(a b) '(1 2))
                    '(a 1 b 2))
      (check-equal? (flatmap2 (lambda (x y) (list x y)) '() '()) '()))

    (test-case "get-values"
      (let ()
        (define test-nested '((a (b 1 2) (c 3)) (d 4)))
        (check-equal? (get-values test-nested '(a b)) '(1 2))
        (check-equal? (get-values test-nested '(a c)) '(3))
        (check-equal? (get-values test-nested '(d)) '(4))
        (check-equal? (get-values test-nested '(a x)) '())
        (check-equal? (get-values test-nested '()) '())
        (check-equal? (get-values '() '(a)) '())))

    (test-case "set-value"
      (check-equal? (set-value '((a (b 1))) '(a b) 2) '((a (b 2))))
      (check-equal? (set-value '((a (b 1))) '(a) 99) '((a 99)))
      (check-equal? (set-value '((a (b 1))) '(a b) '(2 3)) '((a (b (2 3)))))
      (check-equal? (set-value '((a (b 1))) '(c) 5) '((a (b 1)))))

    (test-case "append-value"
      (check-equal? (append-value '((a (b 1))) '(a b) 2) '((a (b (1 2)))))
      (check-equal? (append-value '((a (b 1))) '(a b) '(3 4)) '((a (b ((1) (3 4)))))))

    (test-case "collect-tagged-code"
      (let ()
        (define test-tree '((in x) (out y) (in (z w)) (foo (in bar))))
        (check-equal? (collect-tagged-code 'in test-tree)
                      '((in x) (in (z w)) (in bar)))))

    (test-case "walk"
      (define (id x) x)
      (check-equal? (walk '(a (b c)) atom? id identity 10) '(a (b c)))
      (define (replace-leaf s) (if (symbol? s) 1 s))
      (check-equal? (walk '(a (b c)) atom? replace-leaf identity 10) '(1 (1 1))))

    (test-case "walk-replace"
      (let ()
        (define test-sexp '(f x y (g x)))
        (define bindings '((x a) (y b)))
        (check-equal? (walk-replace test-sexp bindings) '(f a b (g a)))))

    (test-case "rename"
      (let ()
        (define rename-data '((a x y) (b p q)))
        (define rename-schema '(((a) new-a) ((b) new-b)))
        (define renamed (rename rename-data rename-schema))
        (check-equal? (car renamed) '((a new-a0 new-a1) (b new-b0 new-b1)))
        (check-equal? (cadr renamed) '((x new-a0) (y new-a1) (p new-b0) (q new-b1)))))

    (test-case "symbol-replace"
      (check-equal? (symbol-replace 'foo-bar "bar" 'baz) 'foo-baz)
      (check-equal? (symbol-replace 'abc "x" 'y) 'abc)))

   ;; =========================================================================
   ;; 二进制处理函数
   ;; =========================================================================
   (test-suite
    "二进制处理"

    (test-case "bits->string"
      (check-equal? (bits->string 42 8) "00101010")
      (check-equal? (bits->string 0 4) "0000")
      (check-equal? (bits->string 15 4) "1111"))

    (test-case "string->bits"
      (check-equal? (string->bits "1010") 10)
      (check-equal? (string->bits "00101010") 42)
      (check-equal? (string->bits "0") 0))

    (test-case "bits-extract"
      (check-equal? (bits-extract #b110110 2 3) 5)
      (check-equal? (bits-extract #b1111 0 2) 3)
      (check-equal? (bits-extract #b1010 3 1) 1))

    (test-case "bits-split"
      (let ()
        (define num #b110110)
        (check-equal? (bits-split num '((low 0 2) (mid 2 2) (high 4 2)))
                      '((low . 2) (mid . 1) (high . 3)))
        (check-equal? (bits-split num '((0 2) (2 2) (4 2)))
                      '((0 . 2) (1 . 1) (2 . 3)))))

    (test-case "mcode"
      (check-equal? (mcode "11001010" "7:4|3:0") "11001010")
      (check-equal? (mcode "1010" "3:2|1:0") "1010")
      (check-equal? (mcode "1010" "0") "0"))

    (test-case "immcode"
      (check-equal? (immcode 42) "00000000000000000000000000101010")
      (check-equal? (immcode 42 16) "0000000000101010")
      (check-equal? (immcode "1010") "1010"))

    (test-case "int->bytes"
      (check-equal? (int->bytes #x1234 2) #"\x12\x34")
      (check-equal? (int->bytes #x12 1) #"\x12")
      (check-equal? (int->bytes 0 4) #"\x00\x00\x00\x00"))

    (test-case "int->bytes/little"
      (check-equal? (int->bytes/little #x1234 2) #"\x34\x12")
      (check-equal? (int->bytes/little #x12 1) #"\x12"))

    (test-case "pad-left-zero"
      (check-equal? (pad-left-zero "101" 5) "00101")
      (check-equal? (pad-left-zero "101" 3) "101")
      (check-equal? (pad-left-zero "" 2) "00"))

    (test-case "write-bits"
      (check-equal? (write-bits "1010" "1100") "10101100")
      (check-equal? (write-bits) ""))

    (test-case "reg"
      (check-equal? (reg 3 5) "00011")
      (check-equal? (reg 0 3) "000"))

    (test-case "offsetHi / offsetLo"
      (check-equal? (offsetHi #x12345) #x12)
      (check-equal? (offsetHi #xFFFFF) #xFF)
      (check-equal? (offsetLo #x12345) #x345)
      (check-equal? (offsetLo #xFFFFF) #xFFF)))

   ;; =========================================================================
   ;; 扫描器
   ;; =========================================================================
   (test-suite
    "扫描器"

    (test-case "扫描特定符号"
      (let ()
        (define test-sexp '(a (b c) (d (a e)) a))
        (define result-hash (scanner test-sexp (lambda (x) (eq? x 'a))))
        (define paths (hash-ref result-hash 'a))
        (check-not-false (member '(0) paths))
        (check-not-false (member '(2 1 0) paths))
        (check-not-false (member '(3) paths))
        (check-equal? (length paths) 3)))

    (test-case "扫描不存在的符号"
      (let ()
        (define empty-hash (scanner '(x y) (lambda (x) (eq? x 'z))))
        (check-false (hash-has-key? empty-hash 'z)))))

   ;; =========================================================================
   ;; 定位器
   ;; =========================================================================
   (test-suite
    "定位器"

    (test-case "相对路径计算(含占位符)"
      (let ()
        (define tree '(a (b c) hole (d e)))
        (define (hole? x) (eq? x 'hole))
        (check-equal? (location tree '(0) '(3 0) hole?) '(up 2 0))
        (check-equal? (location tree '(0) '(2) hole?) '(up (phys 2)))
        (check-equal? (location tree '(1) '(3) hole?) '(up 2)))))

   ;; =========================================================================
   ;; bind-call
   ;; =========================================================================
   (test-suite
    "bind-call"

    (let ()
      (define func-no-params '(function foo body))
      (define func-in-only '(function foo (in (x y)) body))
      (define func-out-only '(function foo (out (a b)) body))
      (define func-both '(function foo (in (x y)) (out (r s)) body))

      (test-case "无参数调用"
        (check-equal? (bind-call func-no-params '(call foo)) '())
        (check-exn exn:fail? (lambda () (bind-call func-no-params '(call foo (1 2))))))

      (test-case "只有输入参数"
        (check-equal? (bind-call func-in-only '(call foo (1 2))) '((x . 1) (y . 2)))
        (check-exn exn:fail? (lambda () (bind-call func-in-only '(call foo (1)))))
        (check-exn exn:fail? (lambda () (bind-call func-in-only '(call foo (1 2) (r))))))

      (test-case "只有输出参数"
        (check-equal? (bind-call func-out-only '(call foo (r s))) '((a . r) (b . s)))
        (check-exn exn:fail? (lambda () (bind-call func-out-only '(call foo)))))

      (test-case "既有输入又有输出"
        (check-equal? (bind-call func-both '(call foo (1 2) (r s)))
                      '((x . 1) (y . 2) (r . r) (s . s)))
        (check-exn exn:fail? (lambda () (bind-call func-both '(call foo (1 2))))))

      (test-case "函数名不匹配"
        (check-exn exn:fail? (lambda () (bind-call func-both '(call bar (1 2) (r s))))))))

   ;; =========================================================================
   ;; match-signature
   ;; =========================================================================
   (test-suite
    "match-signature"

    (let ()
      (define sig1 '(foo #:in in-var #:out out-var body-var))
      (define sig2 '(bar #:in in-var body-var))
      (define sig3 '(baz #:out out-var body-var))
      (define sig4 '(qux body-var))

      (define inst1 '(foo (in (1 2)) (out (3 4)) (add 1 2) (sub 3 4)))
      (define inst2 '(bar (in (a b)) (add a b)))
      (define inst3 '(baz (out (r s)) (mul r s)))
      (define inst4 '(qux (halt)))

      (test-case "完整签名匹配"
        (check-equal? (match-signature sig1 inst1)
                      '((in-var (in (1 2))) (out-var (out (3 4))) (body-var ((add 1 2) (sub 3 4))))))

      (test-case "只有 in 的签名"
        (check-equal? (match-signature sig2 inst2)
                      '((in-var (in (a b))) (body-var ((add a b))))))

      (test-case "只有 out 的签名"
        (check-equal? (match-signature sig3 inst3)
                      '((out-var (out (r s))) (body-var ((mul r s))))))

      (test-case "无 in/out 的签名"
        (check-equal? (match-signature sig4 inst4)
                      '((body-var ((halt))))))

      (test-case "函数名不匹配"
        (check-exn exn:fail? (lambda () (match-signature sig1 inst2))))

      (test-case "缺少 in 子句"
        (define inst-missing-in '(foo (out (3 4)) (add 1 2)))
        (check-exn exn:fail? (lambda () (match-signature sig1 inst-missing-in))))

      (test-case "body 部分允许多个表达式"
        (define inst-extra '(foo (in (1 2)) (out (3 4)) (extra) (add 1 2)))
        (check-equal? (match-signature sig1 inst-extra)
                      '((in-var (in (1 2))) (out-var (out (3 4))) (body-var ((extra) (add 1 2))))))))

   ;; =========================================================================
   ;; utils-namespace
   ;; =========================================================================
   (test-suite
    "utils-namespace"

    (test-case "eval 基本功能"
      (define ns (utils-namespace))
      (check-equal? (eval '(cons 1 2) ns) '(1 . 2))
      (check-equal? (eval '(atom? 'a) ns) #t)
      (check-equal? (eval '(bits->string 42 8) ns) "00101010")))))

;; 运行所有测试
(run-tests all-tests)