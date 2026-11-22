#lang racket

;; ============================================================================
;; 寄存器分配器 (issue #7)
;; ============================================================================
;; 本模块只实现机制,不预设任何目标平台或规则文件的约定。
;; 以下约定全部由调用方(规则文件)显式传入:
;;   - 寄存器池与各角色寄存器表   -> register-pool / allocate-registers
;;   - 寄存器符号的前缀字符       -> register-symbol? / return-registers
;;   - 调用指令标签               -> instruction-effects / live-intervals
;;   - 控制结构标签               -> live-intervals
;;   - return 标签                -> return-registers
;;
;; 采用的基本方法:活跃区间 + 线性扫描 + 调用感知。
;;   1. live-intervals 为变量计算 [入口, 最后使用] 区间,并标记是否跨越调用;
;;   2. allocate-registers 按区间起点扫描,为变量挑选物理寄存器:
;;      - 不跨越调用的变量保留入口实参寄存器(caller-saved);
;;      - 跨越调用的变量只能使用调用方传入的被调用者保存寄存器池;
;;      - 保存寄存器不足时分配到栈槽(spill),由规则层决定如何处理;
;;      - 跨越调用且分配到保存寄存器的变量生成入口搬移指令(move)。
;;
;; 说明:控制结构头部中的变量会在每一轮循环/分支中重新参与条件判断,
;; 因此在节点末尾补记一次使用,保证跨越循环体内调用的活跃变量不会被误判为短生命周期。
;; 这是保守近似:可能多分配保存寄存器,但不会漏掉跨调用活跃值。

(provide register-pool
         instruction-effects
         live-intervals
         allocate-registers
         allocation-ref
         interval-var
         interval-start
         interval-end
         interval-cross?
         interval-pref
         matching-symbols
         register-symbol?
         registers-in
         return-registers
         unique)

;; ============================================================================
;; 基础工具
;; ============================================================================

;; 关联表取值,找不到返回默认值
(define (alist-ref key alist [default '()])
  (let ((p (assoc key alist)))
    (if p (cdr p) default)))

;; 保序去重
(define (unique lst)
  (let loop ((rest lst) (acc '()))
    (cond
      [(null? rest) (reverse acc)]
      [(member (car rest) acc) (loop (cdr rest) acc)]
      [else (loop (cdr rest) (cons (car rest) acc))])))

;; ============================================================================
;; 寄存器池(结构由调用方定义)
;; ============================================================================

;; pools 是关联表,形如 ((角色名 (寄存器 ...)) ...)。
;; 本模块不规定有哪些角色,也不规定每个角色有哪些寄存器或符号名。
(define (register-pool pools kind [default '()])
  (alist-ref kind pools default))

;; ============================================================================
;; 符号扫描
;; ============================================================================

;; 寄存器符号:首字母属于 prefixes,其余均为数字。
;; prefixes 可以是字符串或字符列表,具体取值由调用方(规则文件)定义。
(define (register-symbol? sym prefixes)
  (define chars (if (string? prefixes) (string->list prefixes) prefixes))
  (and (symbol? sym)
       (let ((s (symbol->string sym)))
         (and (> (string-length s) 1)
              (member (string-ref s 0) chars)
              (let loop ([i 1])
                (cond
                  [(= i (string-length s)) #t]
                  [(char-numeric? (string-ref s i)) (loop (+ i 1))]
                  [else #f]))))))

;; 收集 sexp 中所有满足 pred? 的符号(保序去重)
(define (matching-symbols sexp pred?)
  (define found '())
  (define (walk node)
    (cond
      [(symbol? node)
       (when (and (pred? node) (not (member node found)))
         (set! found (cons node found)))]
      [(pair? node)
       (walk (car node))
       (walk (cdr node))]
      [else (void)]))
  (walk sexp)
  (reverse found))

;; 返回 pool 中出现在 sexp 里的寄存器(按 pool 顺序)
(define (registers-in sexp pool)
  (define present (matching-symbols sexp (lambda (s) (member s pool))))
  (filter (lambda (r) (member r present)) pool))

;; 收集 (return-tag reg ...) 中出现的寄存器符号。
;; prefixes 与 return-tag 均由规则文件定义。
(define (return-registers body prefixes return-tag)
  (define found '())
  (define (walk node)
    (cond
      [(pair? node)
       (if (and (symbol? (car node)) (eq? (car node) return-tag))
           (begin
             (set! found (append found
                                 (matching-symbols (cdr node)
                                                   (lambda (s)
                                                     (register-symbol? s prefixes)))))
             (walk (cdr node)))
           (begin
             (walk (car node))
             (walk (cdr node))))]
      [else (void)]))
  (walk body)
  (unique found))

;; ============================================================================
;; 指令副作用(活跃分析的底层信息)
;; ============================================================================

;; 返回 (list tag direct-symbols is-call?)。
;; call-tags 由调用方(规则文件)定义,为代表调用指令的标签列表。
(define (instruction-effects node call-tags)
  (if (and (pair? node) (symbol? (car node)))
      (let ((tag (car node)))
        (list tag
              (filter symbol? (cdr node))
              (and (member tag call-tags) #t)))
      (list #f '() #f)))

;; ============================================================================
;; 活跃区间
;; ============================================================================

;; 区间表示为列表:(var start end crosses-call? preferred)
;;   var       -> 变量名(符号)
;;   start     -> 起点(参数恒为 0,函数入口即存在)
;;   end       -> 最后一次使用的位置
;;   crosses?  -> 是否跨越至少一个调用
;;   preferred -> 首选寄存器(入口实参寄存器)
(define (interval-var iv) (list-ref iv 0))
(define (interval-start iv) (list-ref iv 1))
(define (interval-end iv) (list-ref iv 2))
(define (interval-cross? iv) (list-ref iv 3))
(define (interval-pref iv) (list-ref iv 4))

;; 计算 vars 中每个变量的活跃区间。
;;   call-tags     -> 调用指令标签(规则文件定义)
;;   control-tags  -> 控制结构标签(规则文件定义),头部变量在节点末尾补记使用
;;   arg-regs      -> 入口实参寄存器表,按形参顺序排列(规则文件定义)
(define (live-intervals body vars call-tags control-tags arg-regs)
  (define last-use (make-hash))
  (define call-positions '())
  (define counter 0)

  (define (note-use! sym pos)
    (when (member sym vars)
      (hash-update! last-use sym (lambda (old) (max old pos)) pos)))

  (define (walk node)
    (cond
      [(pair? node)
       (define pos counter)
       (set! counter (add1 counter))
       (define eff (instruction-effects node call-tags))
       (when (third eff)
         (set! call-positions (cons pos call-positions)))
       (define tag (first eff))
       (walk (car node))
       (walk (cdr node))
       ;; 控制结构:节点内出现过的变量在节点末尾补记一次使用
       (when (and tag (member tag control-tags))
         (for-each (lambda (s) (note-use! s counter))
                   (matching-symbols node (lambda (s) (member s vars)))))]
      [(symbol? node)
       (define pos counter)
       (set! counter (add1 counter))
       (note-use! node pos)]
      [else
       (set! counter (add1 counter))]))

  (walk body)
  (define first-call (if (null? call-positions) +inf.0 (apply min call-positions)))
  (for/list ((v vars) (i (in-naturals)))
    (define last (hash-ref last-use v -1))
    (define crosses (and (>= last 0) (< first-call last)))
    (list v
          0
          last
          crosses
          (if (< i (length arg-regs)) (list-ref arg-regs i) #f))))

;; ============================================================================
;; 线性扫描分配
;; ============================================================================

;; 分配结果为关联表:
;;   (regs  (var phys-reg) ...)   变量 -> 物理寄存器(含入口实参寄存器)
;;   (saved phys-reg ...)         分配器实际占用的被调用者保存寄存器
;;   (moves (dst src) ...)        函数入口需要插入的搬移指令
;;   (slots (var offset) ...)     溢出到栈的变量及其槽号
;;   (stack-size n)               需要的栈槽数量
;;
;; saved-pool 是被调用者保存寄存器表(规则文件定义),按分配优先级排列。
(define (allocate-registers intervals saved-pool [reserved '()] [slot-base 0])
  ;; 过滤掉正文中显式使用/保留的寄存器,避免与程序员可见的寄存器冲突
  (define pool (filter (lambda (r) (not (member r reserved))) saved-pool))

  (define (choose-saved used)
    (let loop ((rest pool))
      (cond
        [(null? rest) #f]
        [(member (car rest) used) (loop (cdr rest))]
        [else (car rest)])))

  ;; 所有参数在入口同时存在,不做区间重叠复用,避免入口搬移互相覆盖。
  (define reg-map '())
  (define saved-used '())
  (define moves '())
  (define slots '())
  (define next-slot slot-base)

  (for-each
   (lambda (iv)
     (define v (interval-var iv))
     (define pref (interval-pref iv))
     (if (interval-cross? iv)
         (let ((r (choose-saved saved-used)))
           (if r
               (begin
                 (set! saved-used (cons r saved-used))
                 (set! reg-map (cons (list v r) reg-map))
                 (when pref
                   (set! moves (cons (list r pref) moves))))
               (begin
                 (set! slots (cons (list v next-slot) slots))
                 (set! next-slot (add1 next-slot)))))
         (set! reg-map (cons (list v pref) reg-map))))
   intervals)

  (list (cons 'regs  (reverse reg-map))
        (cons 'saved (reverse saved-used))
        (cons 'moves (reverse moves))
        (cons 'slots (reverse slots))
        (cons 'stack-size (- next-slot slot-base))))

;; 读取分配结果中的某一项
(define (allocation-ref alloc key [default '()])
  (alist-ref key alloc default))
