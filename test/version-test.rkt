#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         racket/string
         "../type.rkt")

(define-runtime-path info-path "../info.rkt")

;; ============================================================================
;; 测试辅助
;; ============================================================================

;; 捕获 thunk 的标准输出。
(define (capture-output thunk)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (thunk))
  (get-output-string out))

;; ============================================================================
;; 版本与 UI 测试
;; ============================================================================

(define version-tests
  (test-suite
   "Version and UI Tests"

   (test-case "*type-version* 从 info.rkt 读取且格式正确"
     (check-true (string? *type-version*))
     (check-true (regexp-match? #px"^[0-9]+\\.[0-9]+\\.[0-9]+$"
                                *type-version*))
     (check-true (string-contains?
                  (file->string info-path)
                  (format "(define version \"~a\")" *type-version*))))

   (test-case "print-version 输出 TYPE v<版本号>"
     (check-equal? (string-trim (capture-output
                                 (lambda () (print-version))))
                   (format "TYPE v~a" *type-version*)))

   (test-case "print-usage 包含所有命令行选项"
     (define usage (capture-output (lambda () (print-usage))))
     (check-true (string-contains? usage "--version"))
     (check-true (string-contains? usage "-v"))
     (check-true (string-contains? usage "--help"))
     (check-true (string-contains? usage "-h"))
     (check-true (string-contains? usage "--build"))
     (check-true (string-contains? usage "-b"))
     (check-true (string-contains? usage "--verbose"))
     (check-true (string-contains? usage *type-version*)))

   (test-case "print-welcome 包含项目名、版本号和使用提示"
     (define welcome (capture-output (lambda () (print-welcome))))
     (check-true (string-contains? welcome "TYPE"))
     (check-true (string-contains? welcome *type-version*))
     (check-true (string-contains? welcome "translate")))

   (test-case "parse-args 识别版本、帮助与 REPL 参数"
     (check-equal? (parse-args '("--version")) 'version)
     (check-equal? (parse-args '("-v")) 'version)
     (check-equal? (parse-args '("--help")) 'help)
     (check-equal? (parse-args '("-h")) 'help)
     (check-equal? (parse-args '()) 'repl))

   (test-case "parse-args 对未知选项返回 error"
     (check-equal? (parse-args '("--bogus"))
                   (list 'error "--bogus")))

    (test-case "parse-args 识别构建参数"
      (check-equal? (parse-args '("--build" "build.scm"))
                    (list 'build "build.scm"))
      (check-equal? (parse-args '("-b" "build.scm"))
                    (list 'build "build.scm"))
      (check-equal? (parse-args '("--build"))
                    (list 'error "--build"))
      (check-equal? (parse-args '("--verbose"))
                    'repl))))

(run-tests version-tests)
