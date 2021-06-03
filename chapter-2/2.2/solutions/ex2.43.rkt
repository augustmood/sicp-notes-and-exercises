#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; the original one:
; (define (queens board-size)
;   (define (queen-cols k)  
;     (if (= k 0)
;         (list empty-board)
;         (filter
;          (lambda (positions) (safe? k positions))
;          (flatmap
;           (lambda (rest-of-queens)
;             (map (lambda (new-row)
;                    (adjoin-position new-row k rest-of-queens))
;                  (enumerate-interval 1 board-size)))
;           (queen-cols (- k 1))))))
;   (queen-cols board-size))

;; the bad one:
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; this one's running time is n times slower than the previous version...

(define empty-board nil)

(define (adjoin-position r k seq)
  (append seq (list (cons k r))))

(define (safe? k positions)
  (accumulate (lambda (a b) (and a b)) #t
              (let ((filtered-positions (filter (lambda (i) (not (= (car i) k))) positions))
                    (k-position (cdr (car (filter (lambda (i) (= (car i) k)) positions)))))
                (map (lambda (i) (and (not (= k-position (cdr i))) 
                                      (not (= (abs (- k-position (cdr i))) (abs (- k (car i))))))) 
                     filtered-positions))))


