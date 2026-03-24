#lang racket

(provide make-trie trie-insert! trie-search)

(define (trie-node node data)
  (mcons node data))

(define (get-children trie)
  (mcar trie))

(define (get-data trie)
  (mcdr trie))

(define (set-children! trie node)
  (set-mcar! trie node))

(define (set-data! trie data)
  (set-mcdr! trie data))

(define (make-trie)
  (trie-node (make-vector 94 #f)  #f))

(define (char->index ch)
  (- (char->integer ch) 33))

(define (trie-insert! root str data)
  (let loop ([cur root] [chars (string->list str)])
    (if (null? chars)
        (set-data! cur data)
        (let* ([ch (car chars)]
               [idx (char->index ch)]
               [child (vector-ref (get-children cur) idx)])
          (if child
              (loop child (cdr chars))
              (let ([new-node (make-trie)])
                (vector-set! (get-children cur) idx new-node)
                (loop new-node (cdr chars))))))))

(define (trie-search root str)
  (let loop ([cur root] [chars (string->list str)])
    (if (null? chars)
        (get-data cur)
        (let* ([ch (car chars)]
               [idx (char->index ch)]
               [child (vector-ref (get-children cur) idx)])
          (if child
              (loop child (cdr chars))
              #f)))))

