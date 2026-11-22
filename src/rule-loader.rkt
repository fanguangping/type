#lang racket

;; ============================================================================
;; 规则与源码加载器 (rule loader)
;; ============================================================================
;; 统一处理示例构建器中重复出现的注释剥离与 S 表达式读取逻辑。
;; 注释约定:首元素与末元素均为以 * 开头/结尾的符号,例如 (*** 说明 ***)。
;; 只使用基础 Scheme/Racket 过程,兼容 Racket 7.5。

(provide remove-comments
         file->sexps
         file->sexp
         read-source
         read-rules
         relative->path-string
         path-string->path)

;; 安全取第一个/最后一个元素。
(define (safe-first t) (if (null? t) #f (car t)))
(define (safe-last t) (if (null? t) #f (car (reverse t))))

(define (symbol-prefix? sym prefix)
  (and (symbol? sym)
       (string-prefix? (symbol->string sym) prefix)))

(define (symbol-suffix? sym suffix)
  (and (symbol? sym)
       (string-suffix? (symbol->string sym) suffix)))

;; 判断一个 S 表达式是否为注释。
(define (comment? sexp)
  (and (list? sexp)
       (symbol-prefix? (safe-first sexp) "*")
       (symbol-suffix? (safe-last sexp) "*")))

;; 递归遍历 S 表达式并剥除注释节点。ctl-num 为递归深度上限。
(define (remove-comments sexp [ctl-num 5000])
  (define (walk node depth)
    (if (<= depth 0)
        (error 'remove-comments "recursion limit exceeded")
        (if (not (list? node))
            node
            (map (lambda (child) (walk child (- depth 1)))
                 (filter (lambda (child) (not (comment? child))) node)))))
  (walk sexp ctl-num))

;; 读取文件中所有顶层 S 表达式,并剥除注释。
(define (file->sexps path)
  (remove-comments
   (call-with-input-file path
     (lambda (in) (port->list read in)))))

;; 读取文件中的第一个顶层 S 表达式,并剥除注释。
(define (file->sexp path)
  (let ([forms (file->sexps path)])
    (if (null? forms)
        (error 'file->sexp "no S-expression in ~a" path)
        (car forms))))

;; 读取源文件,返回其程序形式,例如 (:program ...)。
(define (read-source path)
  (file->sexp path))

;; 读取规则文件,返回 translate 所需的规则列表,形如 ((rules ...))。
(define (read-rules path)
  (file->sexps path))

;; 把字符串路径转为 path 值。
(define (path-string->path p)
  (if (path? p) p (string->path p)))

;; 若 p 为相对路径,则以 base-dir 为基准解析;绝对路径原样返回字符串。
(define (relative->path-string base-dir p)
  (define path (path-string->path p))
  (path->string
   (if (absolute-path? path)
       path
       (build-path (path-string->path base-dir) path))))
