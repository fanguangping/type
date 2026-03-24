#lang racket

(require "../../src/translate-system.rkt")

(define (translate-program src-dir build-dir src-file rule-files)
  (define (safe f t)
    (if (null? t) t
        (f t)))

  (define (not-list? t) (not (list? t)))

  (define (symbol-prefix? sym prefix)
    (string-prefix? (symbol->string sym) prefix))

  (define (symbol-suffix? sym prefix)
    (string-suffix? (symbol->string sym) prefix))

  (define (walk sexp stop-p leaf-p branch-p ctl-num)
    (if (= ctl-num 0)
        (error "failed")
        (if (stop-p sexp)
            (leaf-p sexp)
            (map (lambda (s) (walk s stop-p leaf-p branch-p (- ctl-num 1)))
                 (branch-p sexp)))))
  
  (define (remove-comments sexp)
    (define (comment? sexp)
      (and (list? sexp)
           (symbol? (safe first sexp))
           (symbol-prefix? (safe first sexp) "*")
           (symbol? (safe last sexp))
           (symbol-suffix? (safe last sexp) "*")))
    (define (walk-filter sexp)
      (walk sexp
            not-list?
            identity
            (lambda (s) (filter (lambda (e) (not (comment? e))) s))
            5000))
    (walk-filter sexp))
  
  (define (fullpath dir filename)
    (path->string (build-path dir filename)))

  (define (file->sexp filepath)
    (remove-comments
     (call-with-input-file filepath
       (lambda (in)
         (port->list read in)))))

  (define source
    (file->sexp (fullpath src-dir src-file)))
  (define rules-list
    (map (lambda (f)
           (file->sexp (fullpath src-dir f)))
         (reverse rule-files)))

  (define stage 0)
  (define output
    (foldr
     (lambda (rules src)
       (with-output-to-file (string-append build-dir "build-stage" (~s stage) ".txt") #:exists 'replace
         (lambda ()
           (pretty-display src)
           (void)))
       (set! stage (add1 stage))
       (translate (first src) (second src) rules))
     (list (car source) '()) rules-list))
  (first output))

;; 多阶段翻译
(translate-program "./"
                   "./build/"
                   "simple-program.scm"
                   '("rules-0.scm"))


