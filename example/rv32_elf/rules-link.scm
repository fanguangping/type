(rules
 (function
  (:code (lst)
         (relocate-program lst (lambda (x) (and (pair? x) (eq? (car x) 'label)))))))
