#lang racket

;; 寄存器分配器测试 (issue #7)
;; 覆盖:活跃区间、调用感知、被调用者保存寄存器分配、溢出、入口搬移,
;;      以及 rules-0.scm 中 end -> a2 硬编码移除后的回归验证。
;;
;; 重要:分配器模块不预设任何规则文件约定。本测试在本地定义与 rules-0.scm
;; 一致的约定(池/前缀/调用标签/控制标签/返回标签),并额外用完全不同的
;; 自定义约定验证分配器是纯机制,不依赖 RV32 或任何规则文件。

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../src/register-allocator.rkt"
         "../src/translate-utils.rkt"
         "../src/translate-system.rkt"
         "../src/rule-loader.rkt")

;; 递归收集满足 pred? 的节点,保留节点本身的结构(不同于 flatten)。
;; 测试文件所在目录,保证 raco test 从任意工作目录运行都能找到示例
(define-runtime-path test-dir ".")

(define (find-nodes pred? node)
  (cond
    [(and (pair? node) (pred? node))
     (cons node (find-nodes pred? (cdr node)))]
    [(pair? node)
     (append (find-nodes pred? (car node))
             (find-nodes pred? (cdr node)))]
    [else '()]))

;; ===========================================================================
;; 规则层提供的约定(测试本地定义,与 rules-0.scm 一致)
;; ===========================================================================
(define test-pools
  (list (cons 'arg   '(a0 a1 a2 a3 a4 a5 a6 a7))
        (cons 'temp  '(t0 t1 t2 t3 t4 t5 t6))
        (cons 'saved '(s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11))))

(define test-prefixes     "sfta")
(define test-call-tags    '(:f :s call))
(define test-control-tags '(!for !while !do-while !if !if-else))
(define test-return-tag   'return)
(define test-arg-regs     '(a0 a1 a2 a3))

(define (test-intervals body vars)
  (live-intervals body vars test-call-tags test-control-tags test-arg-regs))

;; ===========================================================================
;; 寄存器池(结构由规则文件定义,分配器不提供默认池)
;; ===========================================================================
(define pools-tests
  (test-suite
   "寄存器池"

   (test-case "register-pool 读取规则文件定义的池"
              (check-equal? (register-pool test-pools 'arg)
                            '(a0 a1 a2 a3 a4 a5 a6 a7))
              (check-equal? (register-pool test-pools 'temp)
                            '(t0 t1 t2 t3 t4 t5 t6))
              (check-equal? (register-pool test-pools 'saved)
                            '(s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11)))

   (test-case "register-pool 支持完全自定义的池(无 RV32 约定)"
              (define custom
                (list (cons 'in  '(x0 x1))
                      (cons 'out '(y0 y1 y2))))
              (check-equal? (register-pool custom 'in) '(x0 x1))
              (check-equal? (register-pool custom 'out) '(y0 y1 y2))
              (check-equal? (register-pool custom 'saved) '())
              (check-equal? (register-pool custom 'saved '(z0)) '(z0)))))

;; ===========================================================================
;; 符号扫描
;; ===========================================================================
(define symbols-tests
  (test-suite
   "符号扫描"

   (test-case "register-symbol? 使用调用方传入的前缀"
              (check-true (register-symbol? 's0 test-prefixes))
              (check-true (register-symbol? 'a11 test-prefixes))
              (check-true (register-symbol? 't6 test-prefixes))
              (check-false (register-symbol? 'sp test-prefixes))
              (check-false (register-symbol? 'end test-prefixes))
              (check-false (register-symbol? 's0x test-prefixes)))

   (test-case "register-symbol? 可用非 RV32 前缀"
              (check-true (register-symbol? 'x0 "xy"))
              (check-true (register-symbol? 'y12 "xy"))
              (check-false (register-symbol? 's0 "xy"))
              ;; 前缀也可以是字符列表
              (check-true (register-symbol? 'r3 (string->list "r")))
              (check-false (register-symbol? 'a3 (string->list "r"))))

   (test-case "matching-symbols 保序去重"
              (check-equal? (matching-symbols '(a b a (c b)) symbol?)
                            '(a b c)))

   (test-case "registers-in 按池顺序返回"
              (check-equal? (registers-in '(s3 t0 s1 s3) '(s0 s1 s2 s3))
                            '(s1 s3)))

   (test-case "return-registers 只收集规则定义的 return 标签中的寄存器"
              (check-equal? (return-registers '((=+ t0 a0 a1) (return (t0)))
                                              test-prefixes test-return-tag)
                            '(t0))
              (check-equal? (return-registers '((call f) (return (s2 t0)))
                                              test-prefixes test-return-tag)
                            '(s2 t0)))

   (test-case "return-registers 可用自定义标签与前缀"
              (check-equal? (return-registers '((goto (x2 y1)) (return (s0)))
                                              "xy" 'goto)
                            '(x2 y1)))))

;; ===========================================================================
;; 指令副作用与活跃区间
;; ===========================================================================
(define intervals-tests
  (test-suite
   "活跃区间"

   (test-case "instruction-effects 识别调用方传入的调用标签"
              ;; 只返回直接符号参数:((t0 t1) (s1)) 是子列表,不进入直接符号
              (check-equal? (instruction-effects '(:f acc ((t0 t1) (s1)))
                                                 test-call-tags)
                            '(:f (acc) #t))
              (check-false (third (instruction-effects '(=+ t0 a0 a1)
                                                       test-call-tags)))
              (check-false (first (instruction-effects 'end test-call-tags))))

   (test-case "调用前使用完毕的变量不跨调用"
              (define ivs (test-intervals '((<- t0 end)
                                            (:f acc ((t0) (s1))))
                                          '(end)))
              (check-equal? (length ivs) 1)
              (check-false (interval-cross? (car ivs)))
              (check-equal? (interval-pref (car ivs)) 'a0))

   (test-case "调用后仍使用的变量跨调用"
              (define ivs (test-intervals '((:f acc ((t0) (s1)))
                                            (branch> t1 end done))
                                          '(end)))
              (check-true (interval-cross? (car ivs)))
              (check-equal? (interval-start (car ivs)) 0)
              (check-true (> (interval-end (car ivs)) 0)))

   (test-case "控制结构头部的变量在循环体内调用后仍活跃"
              (define ivs
                (test-intervals
                 '((!for t1 start end ((:f acc ((t0 t1) (s1))) (<- t0 s1))))
                 '(start end)))
              (check-true (interval-cross? (first ivs)))
              (check-true (interval-cross? (second ivs)))
              (check-equal? (interval-pref (second ivs)) 'a1))

   (test-case "未使用的变量不跨调用"
              (define ivs (test-intervals '((:f acc ((t0) (s1)))) '(unused)))
              (check-false (interval-cross? (car ivs)))
              (check-equal? (interval-end (car ivs)) -1))

   (test-case "自定义调用/控制标签与入口寄存器完全由调用方决定"
              ;; 这里没有任何 RV32 标签:mycall/loop/p0/p1 都是自定义约定
              (define ivs
                (live-intervals '((loop x (mycall g))) '(x)
                                '(mycall) '(loop) '(p0 p1)))
              (check-equal? (length ivs) 1)
              (check-true (interval-cross? (car ivs)))
              (check-equal? (interval-pref (car ivs)) 'p0)

              ;; 换一组调用标签后,同一段代码不再被识别为调用
              (define ivs2
                (live-intervals '((loop x (mycall g))) '(x)
                                '(othercall) '(loop) '(p0 p1)))
              (check-false (interval-cross? (car ivs2))))))

;; ===========================================================================
;; 线性扫描分配
;; ===========================================================================
(define scanning-tests
  (test-suite
   "线性扫描分配"

   (test-case "不跨调用的变量保留入口实参寄存器"
              (define ivs (test-intervals '((<- t0 end)) '(start end)))
              (define alloc (allocate-registers ivs
                                                (register-pool test-pools 'saved)))
              (check-equal? (allocation-ref alloc 'regs)
                            '((start a0) (end a1)))
              (check-equal? (allocation-ref alloc 'saved) '())
              (check-equal? (allocation-ref alloc 'moves) '()))

   (test-case "跨调用变量分配到保存寄存器并生成入口搬移"
              (define ivs (test-intervals '((:f acc ((t0) (s1)))
                                            (branch> t1 end done))
                                          '(end)))
              (define alloc (allocate-registers ivs
                                                (register-pool test-pools 'saved)
                                                '(s0)))
              (check-equal? (allocation-ref alloc 'regs) '((end s1)))
              (check-equal? (allocation-ref alloc 'saved) '(s1))
              (check-equal? (allocation-ref alloc 'moves) '((s1 a0))))

   (test-case "reserved 中的保存寄存器不会被分配"
              (define ivs (test-intervals '((:f acc ((t0) (s1)))
                                            (branch> t1 end done))
                                          '(end)))
              (define alloc (allocate-registers ivs
                                                (register-pool test-pools 'saved)
                                                '(s0 s1 s2)))
              (check-equal? (allocation-ref alloc 'regs) '((end s3)))
              (check-equal? (allocation-ref alloc 'saved) '(s3)))

   (test-case "保存寄存器耗尽时溢出到栈槽"
              ;; 只保留 s0 可用,第二个跨调用变量必须溢出
              (define ivs
                (list (list 'v0 0 10 #t 'a0)
                      (list 'v1 0 10 #t 'a1)))
              (define reserved
                '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11))
              (define alloc (allocate-registers ivs
                                                (register-pool test-pools 'saved)
                                                reserved))
              (check-equal? (allocation-ref alloc 'saved) '(s0))
              (check-equal? (allocation-ref alloc 'slots) '((v1 0)))
              (check-equal? (allocation-ref alloc 'stack-size) 1))

   (test-case "入口搬移方向为 保存寄存器 <- 入口实参寄存器"
              (define ivs
                (list (list 'v0 0 10 #t 'a0)
                      (list 'v1 0 10 #f 'a1)))
              (define alloc (allocate-registers ivs
                                                (register-pool test-pools 'saved)
                                                '()))
              (check-equal? (allocation-ref alloc 'moves) '((s0 a0)))
              (check-equal? (allocation-ref alloc 'regs)
                            '((v0 s0) (v1 a1))))

   (test-case "保存寄存器池完全由调用方提供(无 RV32 约定)"
              (define ivs (list (list 'v 0 10 #t 'p0)))
              (define alloc (allocate-registers ivs '(y0 y1) '()))
              (check-equal? (allocation-ref alloc 'regs) '((v y0)))
              (check-equal? (allocation-ref alloc 'saved) '(y0))
              (check-equal? (allocation-ref alloc 'moves) '((y0 p0)))

              ;; slot-base 可指定起始槽号
              (define alloc2
                (allocate-registers
                 (list (list 'a 0 10 #t 'p0) (list 'b 0 10 #t 'p1))
                 '(y0) '() 3))
              (check-equal? (allocation-ref alloc2 'slots) '((b 3)))
              (check-equal? (allocation-ref alloc2 'stack-size) 1))))

;; ===========================================================================
;; 集成回归:rules-0.scm 不再硬编码 end -> a2
;; ===========================================================================
(define regression-tests
  (test-suite
   "rules-0.scm 寄存器分配回归"

   (test-case "main 的跨调用形参分配到 s* 且 signature 使用 ABI 实参寄存器"
              (define base
                (build-path test-dir ".." "example" "rv32_elf"))
              (define source (read-source (build-path base "example.scm")))
              (define rules (read-rules (build-path base "rules-0.scm")))
              (define result (translate source '() rules))
              (define code (first result))
              (define global (second result))

              ;; 分配器约定来自规则文件的 mapping,而不是 src/ 中的硬编码
              (define globals global)
              (define pools (second (assoc 'reg-pools globals)))
              (check-equal? (register-pool pools 'saved)
                            '(s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11))
              (check-equal? (second (assoc 'call-tags globals))
                            '(:f :s call))
              (check-equal? (second (assoc 'control-tags globals))
                            '(!for !while !do-while !if !if-else))
              (check-equal? (second (assoc 'reg-prefixes globals)) "sfta")

              ;; 不再出现硬编码 a2(旧实现把 end 指定为 a2)
              (check-false (member 'a2 (flatten code)))

              ;; signature 记录入口 ABI 实参 a0/a1,而非 a0/a2
              (define sigs (second (assoc 'signature global)))
              (define main-sig (assoc 'main sigs))
              (define main-in (second (assoc 'in (cdr main-sig))))
              (check-equal? main-in '(a0 a1))

              ;; end 被替换为被调用者保存寄存器 s3(不再是 a 寄存器)
              (define for-node
                (first (find-nodes (lambda (x) (eq? (car x) '!for)) code)))
              (check-equal? (third for-node) 's2)
              (check-equal? (fourth for-node) 's3)

              ;; 入口搬移已插入:<- s2 a0 / <- s3 a1
              (define moves
                (find-nodes (lambda (x) (eq? (car x) '<-)) code))
              (check-true (and (member '(<- s2 a0) moves) #t))
              (check-true (and (member '(<- s3 a1) moves) #t))

              ;; 序言保存所有用到的 s*
              (define push
                (first (find-nodes (lambda (x) (eq? (car x) 'push-stack))
                                   code)))
              (for ([r '(s0 s1 s2 s3)])
                (check-true (and (member r (second push)) #t))))))

(define alloc-tests
  (test-suite
   "寄存器分配器测试 (issue #7)"
   pools-tests
   symbols-tests
   intervals-tests
   scanning-tests
   regression-tests))

(run-tests alloc-tests)
