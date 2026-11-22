#lang racket

;; ============================================================================
;; 链接工序 (link process)
;; ============================================================================
;; 先按规则翻译一遍(与翻译工序相同),再从结果中提取符号表与重定位项,
;; 写入构建环境的 'symbols / 'relocs,供后续包装工序使用。
;; 本期为最小实现:在单个程序内完成解析;多目标文件的 'targets 已预留。

(require "../translate-system.rkt")
(require "../rule-loader.rkt")
(require "process-utils.rkt")
(require "symbol-tools.rkt")

(provide link-process)

;; emit 选项可以是单个符号或一个符号列表。
(define (emit-symbols emit)
  (cond [(null? emit) '()]
        [(symbol? emit) (list emit)]
        [else emit]))

(define (translate-source env rules-path)
  (define rules (read-rules rules-path))
  (define result (translate (env-ref env 'source)
                            (env-ref env 'global '())
                            rules))
  (env-set (env-set env 'source (first result)) 'global (second result)))

(define (link-process env options)
  (define rules-path
    (resolve-option-path (env-ref env 'base-dir) options 'rules))
  (unless rules-path
    (error 'link-process "missing (rules ...) option"))
  (define translated (translate-source env rules-path))
  (define program (env-ref translated 'source))
  (define emit (emit-symbols (options-ref options 'emit '())))
  (define with-linked (env-set translated 'linked program))
  (define with-symbols
    (if (memq 'symbols emit)
        (env-set with-linked 'symbols (collect-labels program))
        with-linked))
  (if (memq 'relocs emit)
      (env-set with-symbols 'relocs (collect-relocations program))
      with-symbols))
