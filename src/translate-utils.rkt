#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 调试打印函数,可接受任意数量参数,并将它们以 pretty 形式打印
; Input:
;   args -> 任意数量的参数(任意类型)
; Output: 无返回值(打印到标准输出)
; Example:
;   (log-print "value:" 42) => 打印 value: 42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (log-print . args)
  (define debug #f)
  (when debug
    (for-each pretty-display args)
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 判断对象是否为原子(非 pair 且非 null)
; Input:
;   x -> 任意对象
; Output: 如果 x 是原子则返回 #t,否则 #f
; Example:
;   (atom? 'a) => #t
;   (atom? '(a b)) => #f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (atom? x)
  (not (or (pair? x) (null? x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 安全地应用函数 f 到参数 t,如果 t 是空列表则直接返回空列表
; Input:
;   f -> 一个单参数函数
;   t -> 列表
; Output: 如果 t 为空则返回空列表,否则返回 (f t)
; Example:
;   (safe car '(a b c)) => 'a
;   (safe car '()) => '()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (safe f t)
  (if (null? t) t
      (f t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 检查 sexp 的第一个元素是否为指定的符号 sym
; Input:
;   sexp -> 一个列表(S-表达式)
;   sym -> 符号
; Output: 如果 sexp 非空且其 car 等于 sym 则返回 #t,否则 #f
; Example:
;   (tag? '(in x) 'in) => #t
;   (tag? '(out y) 'in) => #f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tag? sexp sym)
  (equal? (safe car sexp) sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 根据变量名和个数生成一系列符号,格式为 var0, var1, ...
; Input:
;   var -> 基础符号
;   size -> 生成的个数
; Output: 符号列表
; Example:
;   (sym-seqs 'x 3) => '(x0 x1 x2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sym-seqs var size)
  (map (lambda (i)
         (string->symbol (string-append (symbol->string var) (~a i))))
       (range size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 对列表的每个元素应用 f,然后将结果列表连接起来(flatten 一层)
; Input:
;   f -> 函数,接受一个元素并返回列表
;   lst -> 列表
; Output: 扁平化后的列表
; Example:
;   (flatmap (lambda (x) (list x x)) '(1 2)) => '(1 1 2 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (flatmap f lst)
  (apply append (map f lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 双列表版本的 flatmap,对 lst1 和 lst2 的对应元素应用 f
; Input:
;   f -> 函数,接受两个参数并返回列表
;   lst1 -> 列表
;   lst2 -> 列表
; Output: 扁平化后的列表
; Example:
;   (flatmap2 list '(a b) '(1 2)) => '((a 1) (b 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (flatmap2 f lst1 lst2)
  (apply append (map f lst1 lst2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 从嵌套列表 nlist 中按照路径 path 获取所有值
;   nlist 是一个形如 ((key value) ...) 的嵌套结构,每个 value 可以是子列表
;   path 是一个符号列表,表示键的层次
; Input:
;   nlist -> 嵌套关联列表
;   path -> 路径列表
; Output: 匹配路径的所有叶子值组成的列表
; Example:
;   (define data '((a (b (1 2) (c 3))) (d 4)))
;   (get-values data '(a b)) => '(1 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-values nlist path)
  (cond
    ((null? nlist) '())
    ((null? path) '())
    (#t
     (let ((e (filter (lambda (t) (equal? (safe car t) (car path))) nlist)))
       (if e
           (if (= (length path) 1)
               (flatmap cdr e)
               (get-values (flatmap cdr e) (cdr path)))
           '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 在嵌套列表 nlist 中设置指定路径的值
; Input:
;   nlist -> 嵌套关联列表
;   path -> 路径列表
;   value -> 新值
; Output: 修改后的新嵌套列表
; Example:
;   (set-value '((a (b 1))) '(a b) 2) => '((a (b 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (set-value nlist path value)
  (define (match-path-node? lst path-node)
    (and (list? lst)
         (tag? lst path-node)))
  (define (update lst pth new-value)
    (cons (if (match-path-node? lst (car pth))
              (list (car lst) new-value)
              lst)
          (cdr pth)))
  (cond
    ((null? nlist) nlist)
    ((null? path) nlist)
    (#t
     (map (lambda (node)
            (let* ((matched (match-path-node? node (car path)))
                   (cs (update node path value))
                   (upd (car cs))
                   (nextp (cdr cs))
                   (ended (null? nextp)))
              (if matched
                  (if ended
                      upd
                      (set-value node nextp value))
                  node)))
          nlist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 向指定路径追加值,将原值和新值组合成列表
; Input:
;   nlist -> 嵌套关联列表
;   path -> 路径列表
;   value -> 要追加的值
; Output: 修改后的新嵌套列表
; Example:
;   (append-value '((a (b 1))) '(a b) 2) => '((a (b (1 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (append-value nlist path value)
  (let ((old (get-values nlist path)))
    (set-value nlist path
               (cond
                 [(null? old) (list value)]
                 [(pair? value)  ; 新值是列表,旧值包装后与之并列
                  (list (if (pair? old) old (list old)) value)]
                 [else  ; 新值是原子,追加到旧值列表后
                  (append (if (pair? old) old (list old)) (list value))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 收集所有以指定 tag 为第一个元素的子列表
; Input:
;   tag -> 符号标记
;   lst -> S-表达式
; Output: 所有匹配的子列表组成的列表
; Example:
;   (collect-tagged-code 'in '((in x) (out y) (in z))) => '((in x) (in z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (collect-tagged-code tag lst)
  (define (walk node)
    (cond
      [(null? node) '()]
      [(pair? node)
       (let ([first (car node)]
             [rest (cdr node)])
         (append
          (if (and (pair? first) (eq? (car first) tag))
              (list first)
              '())
          (walk first)
          (walk rest)))]
      [else '()]))
  (walk lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 通用的树遍历函数,根据停止条件、叶子处理、分支处理进行递归
; Input:
;   sexp -> S-表达式
;   stop-p -> 停止条件谓词
;   leaf-p -> 叶子处理函数
;   branch-p -> 分支处理函数(将节点拆分为子节点列表)
;   ctl-num -> 递归深度控制(防止无限递归)
; Output: 遍历结果
; Example:
;   (walk '(a (b c)) atom? identity identity 10) => 保持不变
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (walk sexp stop-p leaf-p branch-p ctl-num)
  (if (= ctl-num 0)
      (error "failed")
      (if (stop-p sexp)
          (leaf-p sexp)
          (map (lambda (s) (walk s stop-p leaf-p branch-p (- ctl-num 1)))
               (branch-p sexp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 在 S-表达式 sexp 中根据绑定列表进行符号替换
; Input:
;   sexp -> S-表达式
;   bindings -> 绑定列表,每个元素为 (原符号 新符号)
; Output: 替换后的新 S-表达式
; Example:
;   (walk-replace '(f x y) '((x a) (y b))) => '(f a b)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (walk-replace sexp bindings)
  (define (replace-with sexp binding)
    (walk sexp
          atom?
          (lambda (s)
            (if (equal? s (first binding))
                (second binding)
                s))
          identity
          1000))
  (foldr
   (lambda (bd s)
     (replace-with s bd))
   sexp bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 根据重命名方案重命名嵌套列表中的符号
;   rename-schema 是一个列表,每个元素为 (path 新基础符号)
;   从 nlist 中取出路径对应的值,生成新符号列表,然后执行替换
; Input:
;   nlist -> 嵌套关联列表
;   rename-schema -> 重命名方案
; Output: 返回两个值的列表:新列表 和 替换绑定列表
; Example:
;   (rename '((a x y)) '(((a) z))) => (list '((a z0 z1)) '((x z0) (y z1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (rename nlist rename-schema)
  (if (null? nlist)
      (list '() '())
      (let ((bindings
             (flatmap (lambda (cfg)
                        (let* ((src (get-values nlist (car cfg)))
                               (dst (sym-seqs (cadr cfg) (length src))))
                          (map (lambda (s t) (list s t))
                               src
                               dst)))
                      rename-schema)))
        (list (walk-replace nlist bindings)
              bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将符号 sym 中的子字符串 s 替换为 r 的字符串表示,并转回符号
; Input:
;   sym -> 原始符号
;   s -> 要替换的子串
;   r -> 替换的目标符号(其字符串形式)
; Output: 替换后的新符号
; Example:
;   (symbol-replace 'foo-bar "bar" 'baz) => 'foo-baz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (symbol-replace sym s r)
  (string->symbol
   (string-replace
    (symbol->string sym) s (symbol->string r))))

;; ============================================================================
;; 基础位操作函数(整数与字符串转换、位提取)
;; ============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将整数转换为指定位宽的二进制字符串,左补零
; Input:
;   n     -> 整数
;   width -> 目标宽度(位数)
; Output: 长度为 width 的二进制字符串
; Example:
;   (bits->string 42 8) => "00101010"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bits->string n width)
  (~r n #:base 2 #:min-width width #:pad-string "0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将二进制字符串转换为整数
; Input:
;   str -> 由 '0' 和 '1' 组成的字符串
; Output: 对应的整数
; Example:
;   (string->bits "1010") => 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (string->bits str)
  (string->number str 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 从整数中提取连续的位段
;   start 为最低位索引(0 表示最低位),len 为要提取的位数
; Input:
;   n     -> 整数
;   start -> 起始位索引(0 为最低位)
;   len   -> 要提取的位数
; Output: 提取出的位段所表示的整数(已右移对齐到最低位)
; Example:
;   (bits-extract #b110110 2 3) => 提取位 2-4(即 101)=> 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bits-extract n start len)
  (bitwise-and (arithmetic-shift n (- start))
               (- (expt 2 len) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将整数按指定的分割方案拆分为多个部分,每个部分可命名
;   splits 是一个列表,每个元素为 (name start len) 或 (start len)
;   若提供 name,则返回的关联列表中键为 name,否则使用分割索引
; Input:
;   n      -> 整数
;   splits -> 分割描述列表
; Output: 关联列表,每个元素为 (键 . 值整数)
; Example:
;   (bits-split #b110110 '((low 0 2) (mid 2 2) (high 4 2)))
;   => '((low . 2) (mid . 1) (high . 3))   ; 因为 bits: 11 01 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bits-split n splits)
  (let loop ([splits splits] [idx 0] [result '()])
    (if (null? splits)
        (reverse result)
        (let* ([spec (car splits)]
               [name (if (symbol? (car spec)) (car spec) idx)]
               [start (if (symbol? (car spec)) (cadr spec) (car spec))]
               [len   (if (symbol? (car spec)) (caddr spec) (cadr spec))])
          (loop (cdr splits)
                (+ idx 1)
                (cons (cons name (bits-extract n start len)) result))))))

;; ============================================================================
;; 高级二进制构造函数
;; ============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 根据位模式从二进制字符串中提取字段(字符串操作,机器无关)
;   pattern 格式如 "3:0|5|7:4",竖线分隔字段,每个字段可以是单个位或范围(高位:低位)
; Input:
;   off    -> 二进制字符串(如 "10101010")
;   pattern -> 提取模式
; Output: 拼接后的二进制字符串
; Example:
;   (mcode "11001010" "7:4|3:0") => "11001010"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mcode off pattern)
  ;; 获取第i位的字符表示(i 从0开始为最低位)
  (define (get-bit i)
    (string-ref off (- (string-length off) (+ i 1))))

  ;; 处理单个字段(可能是单一位或范围)
  (define (process-field field)
    (define parts (string-split field ":"))
    (if (= (length parts) 1)
        ;; 单一位
        (let ([i (string->number (car parts))])
          (string (get-bit i)))
        ;; 范围:高位到低位
        (let ([high (string->number (car parts))]
              [low  (string->number (cadr parts))])
          (list->string
           (for/list ([i (in-range high (sub1 low) -1)])
             (get-bit i))))))

  ;; 按竖线分割模式,处理每个字段后拼接
  (string-append* (map process-field (string-split pattern "|"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将整数 n 转换为指定宽度的二进制字符串(默认32位)
; Input:
;   n     -> 整数或字符串(如果是字符串则直接返回)
;   width -> 目标宽度(可选,默认32)
; Output: width 位二进制字符串
; Example:
;   (immcode 42) => "00000000000000000000000000101010"
;   (immcode 42 16) => "0000000000101010"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (immcode n [width 32])
  (if (string? n) n
      (bits->string (bitwise-and n (- (expt 2 width) 1)) width)))

(define immcode32 immcode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将整数 n 转换为长度为 byte-count 的大端序字节串
; Input:
;   n         -> 整数
;   byte-count -> 字节数
; Output: 字节串
; Example:
;   (int->bytes 4660 2) => #"\x12\x34"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (int->bytes n byte-count)
  (list->bytes
   (for/list ([i (in-range byte-count)])
     ;; 从最高字节开始提取(大端序)
     (bitwise-and (arithmetic-shift n (* -8 (- byte-count i 1))) #xFF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将整数 n 转换为长度为 byte-count 的小端序字节串
; Input:
;   n         -> 整数
;   byte-count -> 字节数
; Output: 字节串
; Example:
;   (int->bytes/little 4660 2) => #"\x34\x12"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (int->bytes/little n byte-count)
  (list->bytes
   (for/list ([i (in-range byte-count)])
     (bitwise-and (arithmetic-shift n (* -8 i)) #xFF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 在字符串左侧补零至指定长度
; Input:
;   str -> 字符串
;   n   -> 目标长度
; Output: 补零后的字符串
; Example:
;   (pad-left-zero "101" 5) => "00101"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pad-left-zero str n)
  (let ([len (string-length str)])
    (if (>= len n)
        str
        (string-append (make-string (- n len) #\0) str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将多个字符串拼接成一个字符串(用于构建二进制码)
; Input:
;   lst -> 多个字符串参数
; Output: 拼接后的字符串
; Example:
;   (write-bits "1010" "1100") => "10101100"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (write-bits . lst)
  (string-append* lst))

;; 为保持向后兼容,保留旧名称 write-bits-32
(define write-bits-32 write-bits)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 将寄存器编号转换为指定宽度的二进制字符串
; Input:
;   r  -> 整数寄存器号
;   sz -> 宽度(位数)
; Output: 二进制字符串,左补零
; Example:
;   (reg 3 5) => "00011"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reg bits->string)   ; reg 是 bits->string 的别名

;; ============================================================================
;; 特定架构辅助函数(示例,非通用,但保留以供参考)
;; 若需机器无关,请使用上述通用函数自行组合
;; ============================================================================

;; 以下两个函数是针对 RISC-V 32 位地址的特定分割(高20位和低12位)
;; 它们返回整数,而非二进制字符串,若需字符串请结合 bits->string 使用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 返回地址的高 20 位(用于 RISC-V U-type 立即数)
; Input:
;   addr -> 整数地址
; Output: 高20位(右移12位后的值)
; Example:
;   (offsetHi 0x12345) => 0x12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (offsetHi addr)
  (arithmetic-shift addr -12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 返回地址的低 12 位(用于 RISC-V I-type 立即数)
; Input:
;   addr -> 整数地址
; Output: 低12位
; Example:
;   (offsetLo 0x12345) => 0x345
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (offsetLo addr)
  (bitwise-and addr #xFFF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 扫描器
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 扫描 S-表达式,收集所有满足谓词 pred? 的符号的路径
; Input:
;   sexp -> S-表达式
;   pred? -> 谓词函数,接受符号并返回布尔值
; Output: 哈希表,键为符号,值为该符号出现位置的所有物理路径列表
; Example:
;   (scanner '(a (b c) a) (lambda (x) (eq? x 'a))) 
;   => 返回哈希表,其中 'a 对应 '((0) (2))(假设索引从0开始)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (scanner sexp pred?)
  (define result (make-hash))   ; 用于收集结果的哈希表
  (let loop ([sexp sexp] [path '()])
    (cond
      [(and (symbol? sexp) (pred? sexp))
       ;; 找到一个目标符号,将当前路径添加到哈希表中
       (hash-update! result sexp
                     (lambda (prev) (cons (reverse path) prev))
                     '())]
      [(pair? sexp)
       ;; 递归遍历列表的每个元素,索引从 0 开始
       (for ([i (in-naturals)] [elem sexp])
         (loop elem (cons i path)))]
      [else (void)]))  ; 忽略其他类型(数字、字符串等)
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 定位器
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 主函数:计算从源物理路径到目标物理路径的相对路径
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 计算从源物理路径到目标物理路径的相对导航路径,考虑占位符
;   placeholder? 用于判断节点是否为占位符(不计入逻辑索引)
; Input:
;   expr -> 根 S-表达式
;   src-phys -> 源物理路径(索引列表)
;   dst-phys -> 目标物理路径
;   placeholder? -> 占位符判断函数
; Output: 相对路径列表,元素为 'up 或逻辑索引
; Example:
;   假设 expr = '(a (b c) d),占位符? 恒为 #f
;   (location expr '(0) '(2)) => '(1 up?) 实际上需要根据具体实现
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (location expr src-phys dst-phys placeholder?)

  ;; 辅助函数:获取节点在给定父节点下的逻辑索引(忽略占位符)
  ;; 如果物理索引对应占位符,则返回 #f;否则返回逻辑索引
  (define (phys->logical parent phys-idx)
    (define children (if (pair? parent) parent '()))
    (let loop ([i 0] [logical 0])
      (cond
        [(= i phys-idx)
         (if (and (< i (length children)) (placeholder? (list-ref children i)))
             #f
             logical)]
        [(< i (length children))
         (loop (+ i 1)
               (if (placeholder? (list-ref children i))
                   logical
                   (+ logical 1)))]
        [else (error "物理索引超出范围")])))

  ;; 将物理路径转换为逻辑路径,允许最后一步为占位符
  ;; 返回两个值:逻辑路径列表和最终节点
  (define (physical->logical-path expr phys-path)
    (let loop ([node expr] [phys phys-path] [log-path '()])
      (if (null? phys)
          (values log-path node)
          (let* ([idx (car phys)]
                 [children (if (pair? node) node '())]
                 [next-node (list-ref children idx)])
            (if (null? (cdr phys))  ; 最后一步
                (let ([log-idx (phys->logical node idx)])
                  (if log-idx
                      (values (append log-path (list log-idx)) next-node)
                      (values (append log-path (list `(phys ,idx))) next-node)))
                (let ([log-idx (phys->logical node idx)])
                  (if (not log-idx)
                      (error "路径中间包含占位符,无效")
                      (loop next-node (cdr phys) (append log-path (list log-idx))))))))))

  ;; 计算两个逻辑路径的最长公共前缀
  (define (common-prefix a b)
    (cond
      [(or (null? a) (null? b)) '()]
      [(equal? (car a) (car b)) (cons (car a) (common-prefix (cdr a) (cdr b)))]
      [else '()]))
  
  (define-values (src-log src-node) (physical->logical-path expr src-phys))
  (define-values (dst-log dst-node) (physical->logical-path expr dst-phys))
  (define prefix (common-prefix src-log dst-log))
  (define len (length prefix))
  (define src-rest (list-tail src-log len))
  (define dst-rest (list-tail dst-log len))
  ;; 相对路径 = 向上步数 + 向下步数
  (append (make-list (length src-rest) 'up) dst-rest))

;; 主函数:根据函数签名解析调用并绑定参数
;; 调用格式:
;;   (call name)                     - 无 in/out 子句的函数
;;   (call name (arg ...))           - 只有 in 或只有 out 的函数
;;   (call name (arg ...) (var ...)) - 既有 in 又有 out 的函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 根据函数定义解析调用表达式,返回参数绑定列表
;   func-def 格式: (function name [in (param ...)] [out (param ...)] body)
;   call-expr 格式: (call name [args] [vars]),具体见上
; Input:
;   func-def -> 函数定义列表
;   call-expr -> 调用表达式
; Output: 绑定列表,每个元素为 (形参 . 实参) 或 (形参 . 变量)
; Example:
;   (bind-call '(function foo (in (x y)) body) '(call foo (1 2))) 
;   => '((x . 1) (y . 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bind-call func-def call-expr)
  ;; 解析函数定义,返回 (name in-params out-params body has-in? has-out?)
  (define (parse-func-def expr)
    (match expr
      [(list 'function name rest ...)
       (define (parse rest in out has-in? has-out?)
         (match rest
           ['() (error "函数定义缺少 body")]
           [(cons (cons 'in params) rest1)
            (if (not has-in?)
                (let ((real-params (if (and (pair? params) (list? (car params)))
                                       (car params)   ; 处理 (in (x y)) 形式
                                       params)))      ; 处理 (in x y) 形式
                  (parse rest1 real-params out #t has-out?))
                (error "重复的 in 子句"))]
           [(cons (cons 'out params) rest1)
            (if (not has-out?)
                (let ((real-params (if (and (pair? params) (list? (car params)))
                                       (car params)
                                       params)))
                  (parse rest1 in real-params has-in? #t))
                (error "重复的 out 子句"))]
           [else (values name in out (car rest) has-in? has-out?)]))
       (parse rest '() '() #f #f)]
      [_ (error "无效的函数定义")]))
  
  (define-values (fname in-params out-params body has-in? has-out?) (parse-func-def func-def))

  (define (match-call)
    (match call-expr
      [(list 'call cname)
       (unless (eq? fname cname) (error "函数名不匹配"))
       (cond
         [(and (not has-in?) (not has-out?)) '()]
         [else (error "调用格式不匹配:函数需要参数列表")])]
      [(list 'call cname args)
       (unless (eq? fname cname) (error "函数名不匹配"))
       (unless (list? args) (error "参数应为列表"))
       (cond
         [(and has-in? (not has-out?))
          (unless (= (length args) (length in-params))
            (error "输入参数个数不匹配"))
          (for/list ([param in-params] [arg args])
            (cons param arg))]
         [(and has-out? (not has-in?))
          (unless (= (length args) (length out-params))
            (error "输出参数个数不匹配"))
          (for/list ([param out-params] [var args])
            (cons param var))]
         [else (error "调用格式不匹配:函数需要两个列表或无需列表")])]
      [(list 'call cname in-args out-vars)
       (unless (eq? fname cname) (error "函数名不匹配"))
       (unless (and (list? in-args) (list? out-vars))
         (error "调用参数应为列表"))
       (cond
         [(and has-in? has-out?)
          (unless (= (length in-args) (length in-params))
            (error "输入参数个数不匹配"))
          (unless (= (length out-vars) (length out-params))
            (error "输出参数个数不匹配"))
          (append
           (for/list ([param in-params] [arg in-args])
             (cons param arg))
           (for/list ([param out-params] [var out-vars])
             (cons param var)))]
         [else (error "调用格式不匹配:函数不应提供两个列表")])]
      [_ (error "无效的调用表达式")]))
  (match-call))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 根据函数签名匹配函数实例,生成变量绑定列表。签名可包含任意数量的关键字参数,
;              每个关键字对应实例中的一个同名子列表。剩余的实例元素作为 body。
;   签名格式: (name [#:keyword1 var1] [#:keyword2 var2] ... body-var)
;           其中每个 #:keyword 后必须跟一个符号变量,最后必须有一个 body 变量。
;   实例格式: (name [keyword1 (expr...)] [keyword2 (expr...)] ... body-exprs...)
;           其中 keyword 子列表以符号开头,body-exprs 为剩余所有元素。
; Input:
;   signature -> 签名列表
;   instance  -> 实例列表
; Output: 绑定列表,每个元素为 (变量 表达式)
; Example:
;   (match-signature '(foo #:in x #:out y body)
;                    '(foo (in (a b)) (out (c)) (add a b)))
;   => '((x (in (a b))) (y (out (c))) (body ((add a b))))
;
;   (match-signature '(bar #:arg a #:opt b body)
;                    '(bar (arg (1 2)) (opt (3)) (display a) (display b)))
;   => '((a (arg (1 2))) (b (opt (3))) (body ((display a) (display b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (match-signature signature instance)
  ;; 解析签名,返回 (函数名 关键字-变量映射列表 body-变量)
  (define (parse-signature sig)
    (match sig
      [(list name args ...)
       (let loop ([args args] [kw-vars '()] [body-var #f])
         (cond
           [(null? args)
            (if body-var
                (values name (reverse kw-vars) body-var)
                (error "签名中缺少 body 变量"))]
           [(keyword? (car args))  ; 如 #:in
            (if (and (pair? (cdr args)) (symbol? (cadr args)))
                (let* ((kw-str (keyword->string (car args)))
                       (kw (string->symbol kw-str))  ; 转为符号 'in
                       (var (cadr args)))
                  (loop (cddr args) (cons (cons kw var) kw-vars) body-var))
                (error (format "签名中 ~a 后应跟一个符号变量" (car args))))]
           [else
            ;; 剩余部分作为 body 变量,只允许一个符号
            (if body-var
                (error "签名中 body 部分只能有一个变量")
                (loop (cdr args) kw-vars (car args)))]))]
      [_ (error "无效的签名格式")]))

  ;; 解析实例,返回 (函数名 剩余部分列表)
  (define (parse-instance inst)
    (match inst
      [(list name rest ...) (values name rest)]
      [_ (error "无效的实例格式")]))

  (define-values (sig-name sig-kw-vars sig-body-var) (parse-signature signature))
  (define-values (inst-name inst-rest) (parse-instance instance))

  (unless (eq? sig-name inst-name)
    (error (format "函数名不匹配: 签名是 ~a, 实例是 ~a" sig-name inst-name)))

  ;; 从 inst-rest 中提取关键字子列表,剩余部分作为 body
  (define kw-expr-map (make-hasheq))
  (define body-parts '())
  (let loop ((rest inst-rest))
    (cond
      [(null? rest)
       (set! body-parts (reverse body-parts))]
      [(and (pair? (car rest)) (symbol? (caar rest)))
       (let* ((item (car rest))
              (kw (caar rest)))
         (if (assoc kw sig-kw-vars)  ; 如果该关键字在签名中
             (begin
               (hash-set! kw-expr-map kw item)
               (loop (cdr rest)))
             (begin
               (set! body-parts (cons item body-parts))
               (loop (cdr rest)))))]
      [else
       (set! body-parts (cons (car rest) body-parts))
       (loop (cdr rest))]))

  ;; 检查签名中要求的关键字在实例中是否存在
  (for ([kw-var (in-list sig-kw-vars)])
    (define kw (car kw-var))
    (unless (hash-has-key? kw-expr-map kw)
      (error (format "实例中缺少 ~a 子列表" kw))))

  ;; 构建绑定列表,按签名顺序添加关键字绑定,最后添加 body 绑定
  (define bindings
    (append
     (for/list ([kw-var (in-list sig-kw-vars)])
       (define kw (car kw-var))
       (define var (cdr kw-var))
       (list var (hash-ref kw-expr-map kw)))
     (list (list sig-body-var body-parts))))
  bindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; relocate-program: 重定位程序
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (relocate-program program label-definition?)
  ;; 步骤1: 遍历程序,收集标签定义位置和有效指令信息
  (define def-map (make-hash))          ; 标签名 -> 原始索引
  (define effective-indices '())         ; 有效指令的原始索引列表
  (define original->effective (make-hash)) ; 原始索引 -> 有效索引

  (let loop ((i 0) (eff 0))
    (when (< i (length program))
      (let ((item (list-ref program i)))
        (cond
          [(label-definition? item)
           ;; 标签定义,格式应为 (label name)
           (match item
             [(list 'label name)
              (hash-set! def-map name i)]
             [_ (error "无效的标签定义格式" item)])
           (loop (+ i 1) eff)]          ; 不增加有效索引
          [else
           ;; 有效指令
           (set! effective-indices (append effective-indices (list i)))
           (hash-set! original->effective i eff)
           (loop (+ i 1) (+ eff 1))]))))

  ;; 检查是否有标签定义但没有有效指令
  (when (and (not (hash-empty? def-map)) (null? effective-indices))
    (error "程序中没有有效指令,无法重定位标签"))

  ;; 步骤2: 为每个标签定义分配目标有效索引
  (define def->target (make-hash))
  (define used (make-hash))   ; 记录已分配的有效指令的原始索引

  ;; 按原始索引排序标签定义
  (define sorted-defs
    (sort (hash->list def-map)
          (lambda (a b) (< (cdr a) (cdr b)))))

  (for-each (lambda (p)
              (define name (car p))
              (define raw-idx (cdr p))
              ;; 从定义之后查找第一个未被使用的有效指令
              (let loop ((j (+ raw-idx 1)))
                (cond
                  [(>= j (length program))
                   ;; 无有效指令,目标为有效指令总数(末尾)
                   (hash-set! def->target name (length effective-indices))]
                  [else
                   (let ((item (list-ref program j)))
                     (cond
                       [(label-definition? item)
                        (loop (+ j 1))]   ; 跳过其他定义
                       [else
                        (if (hash-has-key? used j)
                            (loop (+ j 1))
                            (begin
                              (hash-set! used j #t)
                              (hash-set! def->target name
                                         (hash-ref original->effective j))))]))])))
            sorted-defs)

  ;; 步骤3: 遍历程序,替换引用,生成新程序及映射
  (define replacement-map (make-hash))
  (define (process-item item raw-idx)
    (cond
      [(label-definition? item)
       ;; 标签定义本身保持不变
       item]
      [(pair? item)
       ;; 指令:递归处理其每个子元素
       (for/list ([sub (in-list item)] [pos (in-naturals)])
         (process-sub sub (list raw-idx pos)))]
      [else
       ;; 原子(如数字、符号)按原样返回
       item]))

  (define (process-sub sub path)
    (cond
      [(pair? sub)
       ;; 子列表继续递归
       (for/list ([s (in-list sub)] [pos (in-naturals)])
         (process-sub s (append path (list pos))))]
      [(symbol? sub)
       ;; 符号:检查是否是标签引用
       (if (hash-has-key? def-map sub)
           (let* ((src-raw-idx (car path))   ; 当前指令的原始索引
                  (src-eff (hash-ref original->effective src-raw-idx))
                  (dst-eff (hash-ref def->target sub))
                  (offset (- dst-eff src-eff))
                  (marker (string->symbol (format "~a:~a" sub offset))))
             (hash-set! replacement-map marker offset)
             marker)
           sub)]
      [else sub]))

  (define new-program
    (for/list ([i (in-naturals)] [item program])
      (process-item item i)))

  new-program)

; --- 为 eval 创建预制命名空间 ---

; 1. 创建一个命名空间锚点。当库被加载时,它会捕获当前的命名空间。
;    这个当前命名空间包含了 racket/base 以及本库自身(正在被加载)。
(define-namespace-anchor my-anchor)

; 2. 定义一个函数来创建并返回一个预制的命名空间。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 创建一个预制的命名空间,包含了 racket 和当前库的所有绑定
;   可用于 eval 的安全执行环境
; Input: 无
; Output: 命名空间对象
; Example:
;   (define ns (utils-namespace))
;   (eval '(cons 1 2) ns) => '(1 . 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (utils-namespace)
  ; 从锚点获取一个干净的空命名空间,但包含了所有已加载模块的声明。
  ; 注意:此时命名空间的标识符映射是空的。
  (define ns (namespace-anchor->empty-namespace my-anchor))

  ;; 获取当前模块的路径(即 my-lib.rkt 的模块路径)
  (define my-path
    (variable-reference->resolved-module-path (#%variable-reference)))

  (namespace-require 'racket ns)
  ;; 将本库加载到新命名空间中
  (namespace-require my-path ns)

  ; 返回这个已经配置好的命名空间
  ns)
