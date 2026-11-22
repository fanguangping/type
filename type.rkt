#lang racket

;; ============================================================================
;; TYPE 统一命令行 / REPL 入口
;; ============================================================================

(require racket/runtime-path)
(require "src/translate-system.rkt")
(require "src/build-system.rkt")

(provide *type-version*
         print-version
         print-usage
         print-welcome
         parse-args
         build
         build-file
         run)

;; 运行时路径:用于在新命名空间中加载入口模块与翻译系统模块。
(define-runtime-path type-module-path "type.rkt")
(define-runtime-path src-dir "src")
(define translate-module-path (build-path src-dir "translate-system.rkt"))

(define program-name "TYPE")

;; ============================================================================
;; UI 格式
;; ============================================================================

;; 输出版本号,格式为 "TYPE v0.1.0"。
(define (print-version [out (current-output-port)])
  (fprintf out "~a v~a~n" program-name *type-version*))

;; 输出命令行用法。
(define (print-usage [out (current-output-port)])
  (fprintf out "~a v~a - translate your programs easily~n~n"
           program-name *type-version*)
  (fprintf out "Usage:~n")
  (fprintf out "  racket type.rkt --version     Show version and exit.~n")
  (fprintf out "  racket type.rkt -v            Same as --version.~n")
  (fprintf out "  racket type.rkt --help        Show this help and exit.~n")
  (fprintf out "  racket type.rkt -h            Same as --help.~n")
  (fprintf out "  racket type.rkt --build FILE  Build from a process-list declaration.~n")
  (fprintf out "  racket type.rkt -b FILE       Same as --build.~n")
  (fprintf out "  racket type.rkt --verbose     Print stages while building.~n")
  (fprintf out "  racket type.rkt               Start REPL with a welcome message.~n~n")
  (fprintf out "REPL hint:~n")
  (fprintf out "  (translate source '() rules)  Translate an S-expression program.~n")
  (fprintf out "  (build-file \"path/to/build.scm\")  Run a build declaration.~n"))

;; REPL 启动欢迎信息:项目名称、版本号与简要提示。
(define (print-welcome [out (current-output-port)])
  (fprintf out "~a v~a - translate your programs easily~n"
           program-name *type-version*)
  (fprintf out "Hint: (translate source '() rules) starts a translation; (build-file ...) runs a build.~n")
  (fprintf out "Type ,help for Racket help, or (print-usage) for TYPE usage.~n"))

;; ============================================================================
;; 参数解析
;; ============================================================================

;; 取命令行选项后面的值,缺失时返回 #f。
(define (option-value args flag)
  (let ([tail (member flag args)])
    (and tail (>= (length tail) 2) (second tail))))

;; 解析命令行参数,返回 'version / 'help / 'repl /
;; (list 'build 声明文件) 或 (list 'error 错误选项)。
(define (parse-args args)
  (cond
    [(or (member "--version" args) (member "-v" args)) 'version]
    [(or (member "--help" args) (member "-h" args)) 'help]
    [(or (member "--build" args) (member "-b" args))
     (let* ([flag (if (member "--build" args) "--build" "-b")]
            [value (option-value args flag)])
       (if (and value
                (string? value)
                (not (member value '("-v" "--version" "-h" "--help"))))
           (list 'build value)
           (list 'error flag)))]
    [(and (member "--verbose" args)
          (null? (remove "--verbose" args)))
     'repl]
    [(null? args) 'repl]
    [else (list 'error (first args))]))

;; ============================================================================
;; 启动 REPL
;; ============================================================================

;; 在包含 TYPE 绑定的干净命名空间中启动 REPL。
(define (start-repl)
  (print-welcome)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket)
    (namespace-require `(file ,(path->string type-module-path)))
    (read-eval-print-loop)))

;; ============================================================================
;; 主入口
;; ============================================================================

(define (run [args (vector->list (current-command-line-arguments))])
  (define action (parse-args args))
  (cond
    [(eq? action 'version) (print-version)]
    [(eq? action 'help) (print-usage)]
    [(eq? action 'repl) (start-repl)]
    [(and (pair? action) (eq? (first action) 'build))
     (define options
       (if (member "--verbose" args) (list (cons 'verbose #t)) '()))
     (build-file (second action) #:options options)
     (void)]
    [else
     (eprintf "Unknown option: ~a~n" (second action))
     (print-usage (current-error-port))
     (exit 1)]))

(module+ main
  (run))
