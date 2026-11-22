#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         "../src/process/process-utils.rkt"
         "../src/process/symbol-tools.rkt"
         "../src/process/translate-process.rkt"
         "../src/process/link-process.rkt"
         "../src/process/encode-process.rkt"
         "../src/process/elf-process.rkt")

;; ============================================================================
;; 测试辅助
;; ============================================================================

(define (with-temp-process thunk)
  (define dir
    (build-path (find-system-path 'temp-dir)
                (format "type-process-test-~a" (random 1000000))))
  (make-directory* dir)
  (dynamic-wind
   void
   (lambda () (thunk dir))
   (lambda () (when (directory-exists? dir) (delete-directory/files dir)))))

(define (write-text path text)
  (with-output-to-file path #:exists 'replace
    (lambda () (display text) (void))))

;; ============================================================================
;; 符号工具测试
;; ============================================================================

(define test-program
  (list ':program
        (list '(global _start)
              '(label main)
              '(call main:17)
              '(label spin)
              '(jump spin:0))))

(define symbol-tool-tests
  (test-suite
   "symbol tool tests"

   (test-case "strip-offset 去掉标签偏移后缀"
     (check-equal? (strip-offset 'main:17) 'main)
     (check-equal? (strip-offset 'spin:0) 'spin))

   (test-case "instruction-form? 区分指令与伪形式"
     (check-false (instruction-form? '(label main)))
     (check-false (instruction-form? '(global _start)))
     (check-false (instruction-form? '(note "x")))
     (check-true (instruction-form? '(call main))))

   (test-case "collect-labels 记录标签字节偏移"
     (check-equal? (collect-labels test-program)
                   '((main 0) (spin 4))))

   (test-case "collect-relocations 记录 call 重定位"
     (check-equal? (collect-relocations test-program)
                   '((0 main 18 0))))))

;; ============================================================================
;; 工序测试
;; ============================================================================

(define process-tests
  (test-suite
   "process handler tests"

   (test-case "env/options 存取关联表"
     (define env '((a . 1)))
     (check-equal? (env-ref env 'a) 1)
     (check-equal? (env-ref env 'b 9) 9)
     (check-equal? (env-ref (env-set env 'a 2) 'a) 2)
     (check-equal? (env-ref (env-set env 'b 3) 'b) 3))

   (test-case "translate-process 按规则翻译源码"
     (with-temp-process
      (lambda (dir)
        (write-text (build-path dir "rules.scm") "(rules (mapping (x y)))")
        (define env
          (translate-process
           (list (cons 'source '(x))
                 (cons 'global '())
                 (cons 'base-dir (path->string dir)))
           (list (cons 'rules "rules.scm"))))
        (check-equal? (env-ref env 'source) '(y)))))

   (test-case "link-process 输出符号与重定位"
     (with-temp-process
      (lambda (dir)
        (write-text (build-path dir "rules.scm") "(rules)")
        (define env
          (link-process
           (list (cons 'source test-program)
                 (cons 'global '())
                 (cons 'base-dir (path->string dir)))
           (list (cons 'rules "rules.scm")
                 (cons 'emit '(symbols relocs)))))
        (check-equal? (env-ref env 'symbols) '((main 0) (spin 4)))
        (check-equal? (env-ref env 'relocs) '((0 main 18 0))))))

   (test-case "encode-process 把二进制字符串编码为字节"
     (define strings
       (list "00000000000000000000000000000001"
             "00000000000000000000000000000010"))
     (define env (encode-process (list (cons 'source strings)) '()))
     (check-equal? (env-ref env 'code) (bytes 1 0 0 0 2 0 0 0)))

   (test-case "encode-process 识别 (:program ...) 形式"
     (define program
       (list ':program
             (list "00000000000000000000000000000001")
             (list ':data)))
     (define env (encode-process (list (cons 'source program)) '()))
     (check-equal? (env-ref env 'code) (bytes 1 0 0 0)))

   (test-case "elf-process 生成目标文件并登记产物"
     (with-temp-process
      (lambda (dir)
        (define env
          (elf-process
           (list (cons 'base-dir (path->string dir))
                 (cons 'code (bytes 1 0 0 0 2 0 0 0))
                 (cons 'symbols '((_start 0)))
                 (cons 'relocs '((0 _start 18 0))))
           (list (cons 'kind 'object)
                 (cons 'output "test.o")
                 (cons 'class 32))))
        (define output (build-path dir "test.o"))
        (check-true (file-exists? output))
        (check-true (> (file-size output) 0))
        (check-equal? (env-ref env 'artifacts)
                      (list (path->string output)))
        (check-equal? (bytes-ref (env-ref env 'elf-object) 4) 1))))

   (test-case "elf-process 生成可执行文件"
     (with-temp-process
      (lambda (dir)
        (define env
          (elf-process
           (list (cons 'base-dir (path->string dir))
                 (cons 'code (bytes 1 0 0 0 2 0 0 0))
                 (cons 'symbols '((_start 0))))
           (list (cons 'kind 'executable)
                 (cons 'output "test.elf")
                 (cons 'class 32)
                 (cons 'entry '_start))))
        (define output (build-path dir "test.elf"))
        (check-true (file-exists? output))
        (check-equal? (bytes-ref (env-ref env 'elf-executable) 4) 1))))))

(run-tests (test-suite "Process tests"
                       symbol-tool-tests
                       process-tests))
