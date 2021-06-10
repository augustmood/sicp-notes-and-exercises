#lang racket
(require "ex2.69.rkt")

(define (generate-frequencies n)
  (define (rec i)
    (if (>= i n)'() (cons (expt 2 i) (rec (+ i 1)))))
  (rec 0))

(define (generate-pairs n)
  (map (lambda (i) (list (string->symbol 
                          (string-upcase 
                           (list->string 
                            (list 
                             (integer->char (inexact->exact (+ 97 (log i 2)))))))) i))
       (generate-frequencies n)))

(generate-huffman-tree (generate-pairs 5))
(newline)
(generate-huffman-tree (generate-pairs 10))

;; for n = 5
;       '(16 8 4 2 1) 32
;         /  \
;       16  '(8 4 2 1) 15
;             / \
;            8  '(4 2 1) 7
;                / \
;               4   '(2 1) 3
;                   / \
;                  2  1

;; 1 bits for the most frequent symbols
;; n - 1 bits for the least frequent symbols