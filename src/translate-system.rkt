#lang racket

(provide translate)

(require "source-processors.rkt")
(require "translate-utils.rkt")
(require "translate-plugins.rkt")
(require "trie.rkt")

(define (bind-rule rule-type sexp)
  (case rule-type
    ('processor
     sexp)
    ('plugin
     (plugin (second sexp)))
    ('source-transformer
     (cons 'lambda (cdr sexp)))
    ('mapping
     (cons global-init-notation sexp))
    ('function
     (cons 'lambda (cdr sexp)))
    ('macro
     sexp)))

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

(define (binding sym local global)
  (log-print "[sym]" sym)
  (let ((lr (assoc sym local)))
    (if lr (second lr)
        (let ((gr (assoc sym global)))
          (if gr (second gr)
              sym)))))

(define (handle-list f nodes local global)
  (define res '())
  (define new-local local)
  (define new-global global)
  (for/list ((n nodes))
    (let ((result (f n new-local new-global)))
      (log-print "[n]" n (first result))
      (if (null? (first result))
          #f
          (set! res (append res (list (first result)))))
      (set! new-local (second result))
      (set! new-global (third result))))
  (log-print "[res]" res)
  (list res new-local new-global))

(define (translate source global rules)
  (log-print "[translate]" source)
  (define trie (build-rules rules))
  (define (rule-selector rule-tag)
    (if (symbol? rule-tag)
        (trie-search trie (symbol->string rule-tag))
        (cons #f '())))
  (define (global-init mappings)
    (first (global-init-handler mappings rule-selector '() '())))
  
  (define (walk-list lst local global ctrl-num)
    (log-print "[walk-list]" lst local global)

    (define (next-walk node local global)
      (walk node local global (sub1 ctrl-num)))

    (define (apply-function-rules lst rule-selector local global)
      (eval-handler lst rule-selector local global))

    (define (apply-macro-rules lst rule-selector local global)
      (let ((r (macro-expand-handler lst rule-selector local global)))
        (next-walk (first r) (second r) (third r))))

    (define (apply-source-transformer-rules lst rule-selector local global)
      (let ((r (eval-handler (list (list source-notation lst source) rule-selector local global))))
        (log-print "[r]" r)
        r))
    
    (if (= ctrl-num 0)
        (error "fail")
        (let* ((tag (car lst))
               (arg (list lst rule-selector local global))
               (plg (plugin tag))
               (rul (rule-selector tag))
               (rtp (if rul (car rul) '()))
               (rct (if rul (cdr rul) '()))
               (x (log-print "[let*]" tag arg plg rul rtp rct)))
          (if plg
              (apply plg arg)
              (if rul
                  (case rtp
                    ('function
                     (apply-function-rules lst rule-selector local global))
                    ('source-transformer
                     (apply-source-transformer-rules lst rule-selector local global))
                    ('macro
                     (apply-macro-rules lst rule-selector local global))
                    (else
                     (log-print "[else]" lst)
                     (handle-list next-walk lst local global)))
                  (handle-list next-walk lst local global))))))
  
  (define (walk-atom atm local global ctrl-num)
    (log-print "[walk-atom]" atm local global)
    (if (= ctrl-num 0)
        (error "fail")
        (list
         (cond
           ((null? atm)
            '())
           ((not (symbol? atm))
            atm)
           (#t
            (binding atm local global)))
         local
         global)))

  (define (process-source src tag)
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

  (define (walk-preprocess node ctrl-num)
    (process-source node 'pre))

  (define (walk-postprocess node ctrl-num)
    (process-source node 'post))

  (define (walk node local global ctrl-num)
    (if (= ctrl-num 0)
        (error "fail")
        (cond
          ((null? node)
           (list '() local global))
          ((list? node)
           (walk-list node local global ctrl-num))
          (#t
           (walk-atom node local global ctrl-num)))))

  (define (walk-util-nochange source global)
    (let ((dst (walk source '() global 100)))
      (if (equal? (first dst) source)
          dst
          (walk-util-nochange (first dst) (third dst)))))

  (define mpps (get-values rules '(rules mapping)))
  
  (let* ((src (walk-preprocess source 100))
         (glb (append global (global-init mpps)))
         (res (walk-util-nochange source glb)))
    (list (walk-postprocess (first res) 100) (third res))))


