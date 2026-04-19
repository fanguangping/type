#lang racket

(require rackunit
         rackunit/text-ui
         "../src/translate-system.rkt"
         "../src/translate-plugins.rkt"
         "../src/trie.rkt")

;; ============================================================================
;; 辅助函数测试
;; ============================================================================

(define (make-test-rule-selector)
  (define trie (make-trie))
  (trie-insert! trie "my-add" '(macro my-add (a b) (=+ result a b)))
  (rule-selector-from-trie trie))

(define translate-system-tests
  (test-suite "Translate System Refactor Tests"
    
    (test-case "rule-selector-from-trie returns correct rule"
      (define selector (make-test-rule-selector))
      (define rule (selector 'my-add))
      (check-equal? (car rule) 'macro)
      (check-equal? (cadr rule) 'my-add))

    (test-case "eval-atom handles symbols, null, and literals"
      (define local '((x 42)))
      (define global '((y 100)))
      (check-equal? (eval-atom 'x local global) 42)
      (check-equal? (eval-atom 'y local global) 100)
      (check-equal? (eval-atom 'z local global) 'z)
      (check-equal? (eval-atom '() local global) '())
      (check-equal? (eval-atom 123 local global) 123))

    (test-case "lookup-binding searches local then global"
      (define local '((a 1) (b 2)))
      (define global '((b 20) (c 30)))
      (check-equal? (lookup-binding 'a local global) 1)
      (check-equal? (lookup-binding 'b local global) 2)
      (check-equal? (lookup-binding 'c local global) 30)
      (check-equal? (lookup-binding 'd local global) 'd))

    ;; 修正:mock-walk 只接受4个参数,handle-list 调用只传4个参数
    (test-case "handle-list traverses nodes sequentially"
      (define (mock-walk node local global depth)
        (list (if (number? node) (+ node 1) node) local global))
      (define result (handle-list mock-walk '(1 2 3) '() '()))
      (check-equal? (first result) '(2 3 4))
      (check-equal? (second result) '())
      (check-equal? (third result) '()))

    (test-case "initialize-global-bindings processes mappings"
      (define mappings '((x (+ 1 2)) (y (* 3 4))))
      (define (dummy-selector tag) #f)
      (define global (initialize-global-bindings mappings dummy-selector))
      (check-equal? (assoc 'x global) '(x 3))
      (check-equal? (assoc 'y global) '(y 12)))

    (test-case "preprocess-source applies processor rules"
      (define rules '((rules (processor (pre (upgrade (lambda (x) x) :<=))))))
      (define src '(a b (:<= (c))))
      (define processed (preprocess-source src rules))
      (check-equal? processed '(a b c)))

    (test-case "postprocess-source applies processor rules"
      (define rules '((rules (processor (post (upgrade (lambda (x) x) :<=))))))
      (define src '(a b (:<= (c))))
      (define processed (postprocess-source src rules))
      (check-equal? processed '(a b c)))

    (test-case "translate function works with empty rules"
      (define src '(+ 1 2))
      (define rules '((rules)))
      (define result (translate src '() rules))
      (check-equal? (first result) '(+ 1 2)))
    ))

(run-tests translate-system-tests)

