(rules
 (processor
  (*** 后处理器 ***)
  (post
   (upgrade (lambda (x) x) :<=)))
 (*** 字典映射 ***)
 (mapping
  (add '+)
  (print 'displayln))
 (*** 宏转换 ***)
 (macro
  (let (x num)
    (define x num))))
