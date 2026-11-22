#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         racket/runtime-path
         racket/string
         "../src/build-system.rkt")

;; ============================================================================
;; 测试辅助
;; ============================================================================

;; 创建临时目录并写入文件。
(define (make-temp-dir)
  (build-path (find-system-path 'temp-dir)
              (format "type-build-test-~a" (random 1000000))))

(define (write-text path text)
  (make-directory* (path-only path))
  (with-output-to-file path #:exists 'replace
    (lambda () (display text) (void))))

(define (with-temp-build thunk)
  (define dir (make-temp-dir))
  (make-directory* dir)
  (dynamic-wind
   void
   (lambda () (thunk dir))
   (lambda () (when (directory-exists? dir) (delete-directory/files dir)))))

;; ============================================================================
;; 选项与注册表
;; ============================================================================

(define option-tests
  (test-suite
   "build option tests"

   (test-case "option-list->alist 处理单值/多值/无值选项"
     (define opts
       (option-list->alist '((rules "r.scm")
                             (emit symbols relocs)
                             (verbose))))
     (check-equal? (cdr (assoc 'rules opts)) "r.scm")
     (check-equal? (cdr (assoc 'emit opts)) '(symbols relocs))
     (check-equal? (cdr (assoc 'verbose opts)) #t))

   (test-case "available-processes 包含四类内置工序"
     (define names (map process-name available-processes))
     (for-each (lambda (name) (check-true (and (memq name names) #t)))
               '(translate link encode wrap-elf)))))

;; ============================================================================
;; build 测试
;; ============================================================================

(define build-tests
  (test-suite
   "build pipeline tests"

   (test-case "build 按工序清单翻译源码"
     (with-temp-build
      (lambda (dir)
        (write-text (build-path dir "source.scm") "(x)")
        (write-text (build-path dir "rules-0.scm") "(rules (mapping (x y)))")
        (define env
          (build "source.scm"
                 '((translate (rules "rules-0.scm")))
                 #:base-dir (path->string dir)))
        (check-equal? (env-ref env 'source) '(y))
        (check-equal? (env-ref env 'artifacts) '()))))

   (test-case "build 输出阶段转储文件"
     (with-temp-build
      (lambda (dir)
        (write-text (build-path dir "source.scm") "(x)")
        (write-text (build-path dir "rules-0.scm") "(rules)")
        (build "source.scm"
               '((translate (rules "rules-0.scm")))
               #:base-dir (path->string dir)
               #:dump-dir "build")
        (check-true (file-exists? (build-path dir "build" "build-stage0.txt")))
        (check-true (string-contains?
                     (file->string (build-path dir "build" "build-stage0.txt"))
                     "(x)"))))

   (test-case "build 对未知工序报错"
     (with-temp-build
      (lambda (dir)
        (write-text (build-path dir "source.scm") "(x)")
        (check-exn exn:fail?
                   (lambda ()
                     (build "source.scm" '((bogus))
                            #:base-dir (path->string dir)))))))

   (test-case "build-file 读取声明文件并执行工序清单"
     (with-temp-build
      (lambda (dir)
        (write-text (build-path dir "source.scm") "(x)")
        (write-text (build-path dir "rules-0.scm") "(rules (mapping (x z)))")
        (write-text
         (build-path dir "build.scm")
         "(build (source \"source.scm\") (stages (translate (rules \"rules-0.scm\"))))")
        (define env
          (build-file (path->string (build-path dir "build.scm"))))
        (check-equal? (env-ref env 'source) '(z))))))))

(run-tests (test-suite "Build system tests"
                       option-tests
                       build-tests))
