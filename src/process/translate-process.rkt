#lang racket

;; ============================================================================
;; 翻译工序 (translate process)
;; ============================================================================
;; 默认工序。读取规则文件,调用 src/translate-system.rkt 的 translate,
;; 把结果写回构建环境的 'source 与 'global。

(require "../translate-system.rkt")
(require "../rule-loader.rkt")
(require "process-utils.rkt")

(provide translate-process)

(define (translate-process env options)
  (define rules-path
    (resolve-option-path (env-ref env 'base-dir) options 'rules))
  (unless rules-path
    (error 'translate-process "missing (rules ...) option"))
  (define rules (read-rules rules-path))
  (define source (env-ref env 'source))
  (define global (env-ref env 'global '()))
  (define result (translate source global rules))
  (env-set (env-set env 'source (first result)) 'global (second result)))
