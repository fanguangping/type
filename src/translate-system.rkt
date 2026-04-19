#lang racket

(provide translate
         eval-atom
         lookup-binding
         handle-list
         preprocess-source
         postprocess-source
         initialize-global-bindings
         rule-selector-from-trie)

(require "source-processors.rkt")
(require "translate-utils.rkt")
(require "translate-plugins.rkt")
(require "trie.rkt")

;; ============================================================================
;; 辅助函数:规则构建与选择
;; ============================================================================

(define (bind-rule rule-type sexp)
  (case rule-type
    ('processor sexp)
    ('plugin (plugin (second sexp)))
    ('source-transformer (cons 'lambda (cdr sexp)))
    ('mapping (cons global-init-notation sexp))
    ('function (cons 'lambda (cdr sexp)))
    ('macro sexp)))

(define (build-rules sexp)
  (define trie (make-trie))
  (if (equal? (caar sexp) 'rules)
      (map (lambda (r)
             (map (lambda (t)
                    (let ((key (car t))
                          (def (bind-rule (car r) t)))
                      (trie-insert! trie (symbol->string key) (cons (car r) def))))
                  (cdr r)))
           (cdar sexp))
      #f)
  trie)

(define (rule-selector-from-trie trie)
  (lambda (rule-tag)
    (if (symbol? rule-tag)
        (trie-search trie (symbol->string rule-tag))
        (cons #f '()))))

;; ============================================================================
;; 辅助函数:全局初始化
;; ============================================================================

(define (initialize-global-bindings mappings rule-selector)
  (first (global-init-handler mappings rule-selector '() '())))

;; ============================================================================
;; 辅助函数:源文件预处理/后处理
;; ============================================================================

(define (process-source-with-rules src tag rules)
  (define ns (make-base-namespace))
  (foldr
   (lambda (p source)
     (let ((proc (processor (first p)))
           (s (apply (eval (second p) ns) (list source)))
           (rule (third p)))
       (apply proc
              (list s
                    (if (symbol? rule) rule
                        (eval rule ns))))))
   src
   (get-values rules (list 'rules 'processor tag))))

(define (preprocess-source source rules)
  (process-source-with-rules source 'pre rules))

(define (postprocess-source source rules)
  (process-source-with-rules source 'post rules))

;; ============================================================================
;; 辅助函数:表达式求值与绑定
;; ============================================================================

(define (lookup-binding sym local global)
  (let ((lr (assoc sym local)))
    (if lr (second lr)
        (let ((gr (assoc sym global)))
          (if gr (second gr)
              sym)))))

(define (eval-atom atm local global)
  (cond
    ((null? atm) '())
    ((not (symbol? atm)) atm)
    (else (lookup-binding atm local global))))

;; ============================================================================
;; 辅助函数:列表遍历与规则应用
;; ============================================================================

(define (handle-list walk-fn nodes local global)
  (define res '())
  (define new-local local)
  (define new-global global)
  (for/list ((n nodes))
    (let ((result (walk-fn n new-local new-global 100)))  ; 只传4个参数
      (unless (null? (first result))
        (set! res (append res (list (first result)))))
      (set! new-local (second result))
      (set! new-global (third result))))
  (list res new-local new-global))

(define (apply-plugin-or-rule node rule-selector local global ctrl-num walk-fn original-source)
  (define tag (car node))
  (define plg (plugin tag))
  (define rul (rule-selector tag))
  (define rtp (if rul (car rul) '()))
  
  (if plg
      (apply plg (list node rule-selector local global))
      (if rul
          (case rtp
            ('function
             (eval-handler node rule-selector local global))
            ('source-transformer
             (eval-handler (list (list source-notation node original-source) rule-selector local global)))
            ('macro
             (let ((r (macro-expand-handler node rule-selector local global)))
               (walk-fn (first r) (second r) (third r) (sub1 ctrl-num))))  ; 只传4个参数
            (else
             (handle-list walk-fn node local global)))
          (handle-list walk-fn node local global))))

;; ============================================================================
;; 主遍历函数(递归)
;; ============================================================================

(define (walk-node node local global ctrl-num rule-selector original-source)
  (if (= ctrl-num 0)
      (error "Translation recursion limit exceeded")
      (cond
        ((null? node)
         (list '() local global))
        ((list? node)
         (define (walk-next n l g d) 
           (walk-node n l g d rule-selector original-source))  ; 只接受4个参数,original-source 由闭包捕获
         (apply-plugin-or-rule node rule-selector local global ctrl-num walk-next original-source))
        (else
         (list (eval-atom node local global) local global)))))

;; ============================================================================
;; 固定点遍历(直到无变化)
;; ============================================================================

(define (fixed-point-walk source global rule-selector original-source)
  (let loop ((current source) (current-global global))
    (let ((result (walk-node current '() current-global 100 rule-selector original-source)))
      (if (equal? (first result) current)
          result
          (loop (first result) (third result))))))

;; ============================================================================
;; 主翻译函数(对外接口)
;; ============================================================================

(define (translate source global rules)
  (log-print "[translate]" source)
  (define trie (build-rules rules))
  (define rule-selector (rule-selector-from-trie trie))
  (define mappings (get-values rules '(rules mapping)))
  (define preprocessed (preprocess-source source rules))
  (define global-env (append global (initialize-global-bindings mappings rule-selector)))
  (define walked (fixed-point-walk preprocessed global-env rule-selector source))
  (define final (postprocess-source (first walked) rules))
  (list final (third walked)))

