#lang racket

;; ============================================================================
;; 构建系统 (build system)
;; ============================================================================
;; type 的构建入口。把「源码 + 工序清单」交给一组统一接口的工序依次执行,
;; 输出最终成品。每道工序是 (env options) -> env 的过程,env 为关联表,
;; 约定键见 src/process/process-utils.rkt。

(require racket/file)
(require "rule-loader.rkt")
(require "process/process-utils.rkt")
(require "process/translate-process.rkt")
(require "process/link-process.rkt")
(require "process/encode-process.rkt")
(require "process/elf-process.rkt")

(provide build
         build-file
         process
         process-name
         process-handler
         available-processes
         option-list->alist
         env-ref)

;; 工序描述:name 为工序清单中使用的名字,handler 为实际过程。
(struct process (name handler) #:transparent)

;; 内置工序注册表。新增工序只需在此登记。
(define available-processes
  (list (process 'translate translate-process)
        (process 'link link-process)
        (process 'encode encode-process)
        (process 'wrap-elf elf-process)))

;; 按名字查找工序处理器。
(define (lookup-process name)
  (let ([found
         (let loop ([ps available-processes])
           (cond [(null? ps) #f]
                 [(eq? (process-name (car ps)) name) (car ps)]
                 [else (loop (cdr ps))]))])
    (if found
        (process-handler found)
        (error 'build "unknown process: ~a" name))))

;; 把工序清单中的选项 ((key val ...) ...) 转为关联表。
;; 单值选项取 cadr,多值选项取整个 cdr,无值选项记为 #t。
;; stages 特殊:其值本身是一组工序声明,即使只有一个也要保持为列表。
(define (option-list->alist options)
  (map (lambda (option)
         (unless (and (pair? option) (symbol? (car option)))
           (error 'build "bad option: ~s" option))
         (cons (car option)
               (cond [(eq? (car option) 'stages) (cdr option)]
                     [(null? (cdr option)) #t]
                     [(null? (cddr option)) (cadr option)]
                     [else (cdr option)])))
       options))

;; 把当前 env 的源程序转储为 build-stageN.txt。
(define (dump-stage env dump-dir index)
  (make-directory* dump-dir)
  (with-output-to-file
   (build-path dump-dir
               (string-append "build-stage" (number->string index) ".txt"))
   #:exists 'replace
   (lambda ()
     (pretty-display
      (list (env-ref env 'source) (env-ref env 'global '())))
     (void))))

;; 依次执行工序清单。
;; source-file : 源文件路径(相对 base-dir)
;; stages      : 工序声明列表,形如 '((translate (rules "...")) ...)
(define (build source-file stages
               #:base-dir [base-dir (current-directory)]
               #:dump-dir [dump-dir #f]
               #:global-options [global-options '()])
  (define source-path (relative->path-string base-dir source-file))
  (define resolved-dump-dir
    (and dump-dir
         (relative->path-string base-dir dump-dir)))
  (define initial-env
    (list (cons 'source (read-source source-path))
          (cons 'global '())
          (cons 'targets '())
          (cons 'artifacts '())
          (cons 'base-dir base-dir)
          (cons 'options global-options)))
  (let loop ([env initial-env] [rest stages] [index 0])
    (if (null? rest)
        env
        (let* ([decl (car rest)]
               [name (car decl)]
               [handler (lookup-process name)])
          (when resolved-dump-dir
            (dump-stage env resolved-dump-dir index))
          (when (options-ref global-options 'verbose #f)
            (fprintf (current-output-port) "[stage ~a] ~a~n" index name))
          (define stage-options (option-list->alist (cdr decl)))
          (define options
            (append stage-options
                    global-options
                    (list (cons 'base-dir base-dir)
                          (cons 'stage-index index)
                          (cons 'stage-name name))))
          (define next-env
            (with-handlers
                ([exn:fail?
                  (lambda (e)
                    (error 'build "stage ~a (~a) failed: ~a"
                           index name (exn-message e)))])
              (handler env options)))
          (loop next-env (cdr rest) (add1 index))))))

;; 从构建声明文件中读取源码与工序清单并执行。
;; 声明文件格式:
;;   (build (source "...") (rules-root "...") (stages (translate ...) ...))
(define (build-file path #:dump-dir [dump-dir #f] #:options [options '()])
  (define path-str (if (path? path) (path->string path) path))
  (define form (file->sexp path-str))
  (unless (and (pair? form) (eq? (car form) 'build))
    (error 'build-file "expected (build ...) in ~a" path-str))
  (define decls (option-list->alist (cdr form)))
  (define (decl-value key)
    (let ([pair (assoc key decls)])
      (and pair (cdr pair))))
  (define source (decl-value 'source))
  (define stages (decl-value 'stages))
  (unless (and source stages)
    (error 'build-file "missing (source ...) or (stages ...) in ~a" path-str))
  (define base-dir
    (let ([declared (decl-value 'base-dir)]
          [from-path (path-only path-str)])
      (cond [declared (if (string? declared) declared (path->string declared))]
            [from-path (path->string from-path)]
            [else (current-directory)])))
  (define reserved '(source stages base-dir dump-dir))
  (define global-options
    (append (filter (lambda (entry) (not (memq (car entry) reserved))) decls)
            options))
  (define declared-dump (decl-value 'dump-dir))
  (build source
         stages
         #:base-dir base-dir
         #:dump-dir (or dump-dir declared-dump)
         #:global-options global-options))
