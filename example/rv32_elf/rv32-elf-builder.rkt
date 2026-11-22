#lang racket

;; ============================================================================
;; rv32_elf 示例构建入口
;; ============================================================================
;; 工序清单已外置到 build.scm,本脚本只负责调用 type 的构建系统。

(require racket/file)
(require racket/runtime-path)
(require "../../src/build-system.rkt")

(define-runtime-path example-dir ".")

(define env
  (build-file (build-path example-dir "build.scm")))

(displayln "Generated:")
(for-each
 (lambda (path)
   (displayln (format "  ~a (~a bytes)"
                      (file-name-from-path path)
                      (file-size path))))
 (env-ref env 'artifacts '()))
