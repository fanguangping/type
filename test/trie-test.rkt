#lang racket
(require rackunit)
(require "../src/trie.rkt")

;; 测试空 Trie
(test-case "Empty trie"
  (define t (make-trie))
  (check-false (trie-search t ""))
  (check-false (trie-search t "a")))

;; 插入单个字符串
(test-case "Insert single string"
  (define t (make-trie))
  (trie-insert! t "hello" 42)
  (check-equal? (trie-search t "hello") 42)
  (check-false (trie-search t "hell"))        ; 前缀不存在
  (check-false (trie-search t "hello1")))     ; 改为不含空格的字符串

;; 插入多个字符串
(test-case "Insert multiple strings"
  (define t (make-trie))
  (trie-insert! t "hello" 42)
  (trie-insert! t "world" 100)
  (trie-insert! t "hi" 200)
  (check-equal? (trie-search t "hello") 42)
  (check-equal? (trie-search t "world") 100)
  (check-equal? (trie-search t "hi") 200)
  (check-false (trie-search t "hell")))

;; 覆盖已存在的键
(test-case "Overwrite existing key"
  (define t (make-trie))
  (trie-insert! t "hello" 42)
  (trie-insert! t "hello" 99)
  (check-equal? (trie-search t "hello") 99))

;; 插入空字符串
(test-case "Empty string"
  (define t (make-trie))
  (trie-insert! t "" 'empty-data)
  (check-equal? (trie-search t "") 'empty-data))

;; 边界字符(ASCII 33 和 126)
(test-case "Boundary characters"
  (define t (make-trie))
  (define c1 (integer->char 33))  ; #\!
  (define c2 (integer->char 126)) ; #\~
  (define str1 (string c1))
  (define str2 (string c2))
  (trie-insert! t str1 "first")
  (trie-insert! t str2 "last")
  (check-equal? (trie-search t str1) "first")
  (check-equal? (trie-search t str2) "last"))

;; 长字符串
(test-case "Long string"
  (define t (make-trie))
  (define long-str (make-string 100 #\a))
  (trie-insert! t long-str "long")
  (check-equal? (trie-search t long-str) "long"))

;; 确保不存在的键返回 #f
(test-case "Non-existent key returns #f"
  (define t (make-trie))
  (trie-insert! t "hello" 42)
  (check-false (trie-search t "hell"))
  (check-false (trie-search t "world")))
