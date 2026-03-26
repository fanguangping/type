#lang racket

(provide plugin
         eval-handler
         macro-expand-handler
         local-bind-handler
         local-append-handler
         global-init-handler
         global-bind-handler
         global-append-handler
         origin-handler
         global-init-notation
         source-notation)

(require "translate-utils.rkt")
(define ns (utils-namespace))

(define (builtin-operator? sym)
  (let ([val (namespace-variable-value sym #t (lambda () #f) ns)])
    (procedure? val)))

; 绑定 global-init-notation 标记
(define global-init-notation '!!)

; 绑定 origin-handler 标记
(define source-notation ':::)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 求值处理器,对给定节点进行求值,处理符号的内置函数、局部/全局绑定,
;       并支持通过 rule-selector 获取规则的定义。
; 输入:
;   node         -> 待求值的 S-表达式节点
;   rule-selector -> 规则定义(如 '(function ...))
;   local        -> 局部绑定列表,形如 ((sym val) ...)
;   global       -> 全局绑定列表,形如 ((sym val) ...)
; 输出:
;   返回三个值的列表:(求值后的节点 更新后的local 更新后的global)
;   注意:该处理器不修改绑定, local 和 global 原样返回。
; 例子:
;   假设 local 中有 (x 10),且 '+' 是内置函数
;   (eval-handler '(+ x 5) some-selector local global)
;   => '(15 local global)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval-handler node rule-selector local global)
  (log-print "[eval-handler]" node local global)
  (define (plain-val? sym)
    (or (null? sym)
        (and (pair? sym) (equal? (car sym) 'quote))
        (and (not (pair? sym)) (not (symbol? sym)))))
  (define (source-tag lst)
    (if (and (pair? lst) (equal? (car lst) source-notation))
        source-notation
        #f))
  (define (tag lst)
    (if (and (pair? lst) (symbol? (car lst)))
        (car lst)
        #f))
  (define (safe f t)
    (if (null? t) t
        (f t)))
  (define (func sym)
    (let ((f (rule-selector sym)))
      (if (and f (equal? (safe car f) 'function))
          (eval (cdr f) ns)
          (if (builtin-operator? sym)
              (eval sym ns)
              #f))))
  (define (binding sym)
    (let ((lr (assoc sym local)))
      (if lr (second lr)
          (let ((gr (assoc sym global)))
            (if gr (second gr)
                sym)))))
  (define (symbol-val sym)
    (if (symbol? sym)
        (if (func sym)
            (func sym)
            (binding sym))
        sym))
  (define (eval-code code)
    ; (displayln code)
    (cond
      ((null? code)
       '())
      ((plain-val? code)
       code)
      ((symbol? code)
       (symbol-val code))
      ((source-tag code)
       (let ((f (rule-selector (car (second code)))))
         (apply
          (apply (eval (cdr f) ns) (third code))
          (cdr (second code)))))
      ((tag code)
       (let* ((func-name (tag code))
              (func-args (cdr code))
              (f (symbol-val (tag code))))
         (if (func (tag code))
             (apply f (map eval-code func-args))
             (map eval-code code))))
      (#t
       (map eval-code code))))
  (list (eval-code node) local global))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 宏展开处理器,根据 rule-selector 获取的宏定义,将宏调用展开为新的表达式。
;              宏定义格式应为 (macro-name pattern . body),其中 pattern 是一个列表,
;              描述了宏调用的结构,body 是零个或多个表达式作为展开模板。
;              模式匹配规则:
;                - 普通符号匹配一个参数,并绑定到该参数的值。
;                - 关键字如 #:in 后面跟一个变量,匹配调用中一个以 'in 开头的子列表
;                  (如 (in ...)),如果存在则绑定变量到该子列表,否则忽略。
;                - 模式中的最后一个符号如果是点列表的一部分(如 . rest),则匹配剩余的所有参数:
;                    * 如果剩余一个参数,则绑定到该原子;
;                    * 如果剩余多个参数,则绑定到这些参数组成的列表;
;                    * 如果没有剩余,则绑定到空列表。
;              生成的绑定用于替换 body 中的变量,得到展开后的代码,并包装在 '(:<= ...) 中。
;              同时将绑定添加到局部环境。
; Input:
;   node         -> 宏调用节点,如 '(macro arg1 arg2)
;   rule-selector -> 单参数函数,接收符号返回对应的宏定义列表
;   local        -> 当前局部绑定列表
;   global       -> 当前全局绑定列表
; Output:
;   返回三个值的列表:(展开后的新节点 更新后的local 原global)
;   其中新节点为 '(:<= 展开代码)
; Example:
;   假设宏定义 '(my-plus (x y) (+ x y))
;   (macro-expand-handler '(my-plus 3 5) selector local global)
;   => '((:<= (+ 3 5)) ((x 3) (y 5)) global)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (macro-expand-handler node rule-selector local global)
  (log-print "[macro-expand-handler]" node local global)
  (define macro-name (car node))
  (define args (cdr node))
  (define rule (rule-selector macro-name))
  (unless (and (pair? rule) (eq? (cadr rule) macro-name))
    (error "宏定义不匹配"))
  (define pattern (caddr rule))
  (define body (cdddr rule))

  (define (match-keyword? kw arg)
    (and (pair? arg) (symbol? (car arg))
         (eq? (car arg) (string->symbol (keyword->string kw)))))

  (define (match-pattern pattern args)
    (let loop ((p pattern) (args args) (bindings '()))
      (cond
        [(null? p)
         (if (null? args)
             (reverse bindings)
             (error (format "多余参数: 剩余 ~a" args)))]
        [(pair? p)
         (let ((first (car p))
               (rest-p (cdr p)))
           (cond
             [(keyword? first)
              ;; 关键字,后面必须跟变量
              (if (not (pair? rest-p))
                  (error "关键字后缺少变量")
                  #f)
              (let ((var (cadr p)))   ; 变量名
                (if (and (not (null? args)) (match-keyword? first (car args)))
                    ;; 匹配到关键字,绑定 var 到该子列表
                    (loop (cddr p) (cdr args) (cons (list var (car args)) bindings))
                    ;; 未匹配到,绑定 var 到空列表
                    (loop (cddr p) args (cons (list var '()) bindings))))]
             [else
              ;; 普通符号
              (if (null? rest-p)
                  ;; 最后一个符号,打包剩余参数
                  (let ((val (cond
                               [(null? args) '()]
                               [(null? (cdr args)) (car args)]
                               [else args])))
                    (reverse (cons (list first val) bindings)))
                  ;; 不是最后一个,必须有一个参数
                  (if (null? args)
                      (error (format "参数不足: 需要 ~a" first))
                      (loop rest-p (cdr args) (cons (list first (car args)) bindings))))]))]
        [else
         (error "无效模式")])))

  (define bindings (match-pattern pattern args))
  (log-print "[macro-expand-handler bindings]" bindings)
  (define expanded (walk-replace body bindings))
  (list (list ':<= expanded) (append local bindings) global))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 将键值对绑定到局部环境,如果键已存在则更新其值,否则追加新绑定。
; 输入:
;   bindings -> 当前绑定列表
;   key      -> 符号键
;   val      -> 值
; 输出:
;   更新后的绑定列表
; 例子:
;   (bind '((a 1)) 'b 2) => '((a 1) (b 2))
;   (bind '((a 1)) 'a 2) => '((a 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bind bindings key val)
  (let ((r (filter (lambda (t) (equal? key (car t))) bindings)))
    (if (not (null? r))
        (map (lambda (t)
               (if (equal? key (car t))
                   (list key val)
                   t))
             bindings)
        (append bindings (list (list key val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 将值附加到局部环境中键对应的值列表后,如果键不存在则创建新列表。
; 输入:
;   bindings -> 当前绑定列表
;   key      -> 符号键
;   val      -> 要附加的值
; 输出:
;   更新后的绑定列表,键对应的值变为列表(原值附加新值)
; 例子:
;   (bind-append '((a (1))) 'a 2) => '((a (1 2)))
;   (bind-append '() 'a 2) => '((a (2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (bind-append bindings key val)
;  (let ((r (filter (lambda (t) (equal? key (car t))) bindings)))
;    (if (not (null? r))
;        (map (lambda (t)
;               (if (equal? key (car t))
;                   (append t (list val))
;                   t))
;             bindings)
;        (append bindings (list (list key val))))))

(define (bind-append bindings key val)
  (let ((r (filter (lambda (t) (equal? key (car t))) bindings)))
    (if (not (null? r))
        (map (lambda (t)
               (if (equal? key (car t))
                   (list key (append (if (pair? (cadr t)) (cadr t)
                                         (list (cadr t)))
                                     (list val)))
                   t))
             bindings)
        (append bindings (list (list key (list val)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 局部变量绑定处理器,将 node 中指定的变量绑定到求值后的值,并存入局部环境。
;       节点格式应为 '(: 变量名 表达式)
; 输入:
;   node         -> 形如 '(: var expr) 的列表
;   rule-selector -> 规则选择函数(此处未使用,但保留接口)
;   local        -> 当前局部绑定
;   global       -> 当前全局绑定
; 输出:
;   返回三个值的列表:('() 更新后的local 原global)
;   其中 '() 表示该节点不产生代码输出,仅更新环境。
; 例子:
;   (local-bind-handler '(: x (+ 1 2)) selector local global)
;   假设局部绑定为空,则返回 '(() ((x 3)) global)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (local-bind-handler node rule-selector local global)
  (log-print "[local-bind-handler]" node local global)
  (define local-bindings
    (bind local
          (second node)
          (first (eval-handler (third node) rule-selector local global))))
  (list '() local-bindings global))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 局部变量附加处理器,将求值后的值附加到局部环境中指定变量的值列表后。
;       节点格式应为 '(:+ 变量名 表达式)
; 输入:
;   node         -> 形如 '(:+ var expr) 的列表
;   rule-selector -> 规则选择函数
;   local        -> 当前局部绑定
;   global       -> 当前全局绑定
; 输出:
;   返回三个值的列表:('() 更新后的local 原global)
; 例子:
;   (local-append-handler '(:+ x 5) selector local global)
;   假设 local 中有 (x (1 2)),则更新为 (x (1 2 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (local-append-handler node rule-selector local global)
  (log-print "[local-append-handler]" node local global)
  (define local-bindings
    (bind-append local
                 (second node)
                 (first (eval-handler (third node) rule-selector local global))))
  (list '() local-bindings global))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 全局变量初始化处理器,对一组节点进行求值,将结果存入全局环境。
;       节点格式为列表,每个元素形如 '(var expr)。
;       依次对每个 expr 求值,并将 (var 值) 添加到全局环境(后面的表达式可引用前面定义的变量)。
; 输入:
;   nodes        -> 一组节点的列表,如 '((x (+ 1 2)) (y 10))
;   rule-selector -> 规则选择函数
;   local        -> 当前局部绑定
;   global       -> 当前全局绑定(初始为空列表)
; 输出:
;   返回三个值的列表:(更新后的global 原local 更新后的global)
;   其中第一个元素与第三个元素相同,均为新的全局环境列表。
; 例子:
;   (global-init-handler '((x (+ 1 2)) (y 10)) selector '() '())
;   => '(((x 3) (y 10)) () ((x 3) (y 10)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (global-init-handler nodes rule-selector local global)
  (log-print "[global-init-handler]" nodes local global)
  (let loop ((nodes nodes) (global-acc global))
    (if (null? nodes)
        (list global-acc local global-acc)   ; 返回 (新全局 原局部 新全局)
        (let* ((n (car nodes))
               (var (first n))                 ; 变量名
               (expr (second n))                ; 表达式
               (val-result (eval-handler expr rule-selector local global-acc))
               (val (first val-result)))       ; 求值结果
          (loop (cdr nodes) (bind global-acc var val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 全局变量绑定处理器,将 node 中指定的变量绑定到求值后的值,并存入全局环境。
;       节点格式应为 '(! 变量名 表达式)
; 输入:
;   node         -> 形如 '(! var expr) 的列表
;   rule-selector -> 规则选择函数
;   local        -> 当前局部绑定
;   global       -> 当前全局绑定
; 输出:
;   返回三个值的列表:('() 原local 更新后的global)
; 例子:
;   (global-bind-handler '(! x (* 2 3)) selector local global)
;   若 global 为空,则返回 '(() local ((x 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (global-bind-handler node rule-selector local global)
  (log-print "[global-bind-handler]" node local global)
  (define global-bindings
    (bind global
          (second node)
          (first (eval-handler (third node) rule-selector local global))))

  (log-print "[global-bind-handler global]" global)
  (log-print "[global-bind-handler global-bindings]" global-bindings)
  
  (list '() local global-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 全局变量附加处理器,将求值后的值附加到全局环境中指定变量的值列表后。
;       节点格式应为 '(!+ 变量名 表达式)
; 输入:
;   node         -> 形如 '(!+ var expr) 的列表
;   rule-selector -> 规则选择函数
;   local        -> 当前局部绑定
;   global       -> 当前全局绑定
; 输出:
;   返回三个值的列表:('() 原local 更新后的global)
; 例子:
;   (global-append-handler '(!+ x 7) selector local global)
;   假设 global 中有 (x (5 6)),则更新为 (x (5 6 7))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (global-append-handler node rule-selector local global)
  (log-print "[global-append-handler]" node local global)
  (define global-bindings
    (bind-append global
                 (second node)
                 (first (eval-handler (third node) rule-selector local global))))
  (list '() local global-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 原始代码处理器,从全局环境中获取指定符号对应的原始代码。
;       节点格式应为 '(::: 符号),其中符号在全局环境中应绑定到某段代码。
; 输入:
;   node         -> 形如 '(::: sym) 的列表
;   rule-selector -> 规则选择函数(未使用)
;   local        -> 当前局部绑定
;   global       -> 当前全局绑定,应包含 (sym 原始代码) 形式的条目
; 输出:
;   返回三个值的列表:(原始代码 原local 原global)
; 例子:
;   若 global 中有 (code1 (add 1 2)),则 (origin-handler '(::: code1) selector local global)
;   => '((add 1 2) local global)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (origin-handler node rule-selector local global)
  (log-print "[origin-handler]" node local global)
  (define origin-source
    (let ((ori (assoc (car node) global)))
      (if ori
          (second ori)
          '())))
  (list origin-source local global))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 说明: 插件分发函数,根据给定的标签返回对应的处理器函数。
; 输入:
;   plugin-tag -> 符号,指定所需处理器的类型
; 输出:
;   若标签匹配,返回对应的处理器函数;否则返回 #f
; 例子:
;   (plugin ':=)  => eval-handler
;   (plugin '>=<) => macro-expand-handler
;   (plugin '!)   => global-bind-handler
;   (plugin 'unknown) => #f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (plugin plugin-tag)
  (case plugin-tag
    (':=
     eval-handler)
    ('>=<
     macro-expand-handler)
    (':
     local-bind-handler)
    (':+
     local-append-handler)
    ('!!
     global-init-handler)
    ('!
     global-bind-handler)
    ('!+
     global-append-handler)
    (':::
     origin-handler)
    (else
     #f)))



