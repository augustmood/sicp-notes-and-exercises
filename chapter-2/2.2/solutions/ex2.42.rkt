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

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

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



;; Printing the n-queens puzzle is not required, I just implemented it for fun.
(define (draw-row-line n)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (cons "+---" result))))
  (append (iter n nil) (list "+")))

(define (draw-chess-line n row)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (cons "|   " result))))
  (list row (append (iter n nil) (list "|"))))

(define (print-list lst)
  (display (fold-left (lambda (x y) (string-append x y)) "" lst)))

(define (print-border n)
  (print-list (draw-row-line n))
  (newline))

(define (print-result n i positions)
  (print-list (cadr (chess (draw-chess-line n i) positions)))
  (newline))

(define (chess lst positions)
  (let ((pos (cdar (filter (lambda (i) (= (car lst) (car i))) positions))))
    (define (iter i result)
      (cond ((> i (length (lst-f lst 2))) result)
            ((= i pos) (iter (+ i 1) (append result (list "| * "))))
            (else (iter (+ i 1) (append result (list (lst-f (lst-f lst 2) i)))))))
    (append (list (lst-f lst 1)) (list (iter 1 nil)))))

(define (print-board n positions)
  (define (iter i)
    (if (> i n)
        (print-border n)
        (begin (print-border n)
               (print-result n i positions)
               (iter (+ i 1)))))
  (iter 1))

(define (lst-f lst n)
  (define (iter n result)
    (if (= n 1)
        (car result)
        (iter (- n 1) (cdr result))))
  (iter n lst))

(define (print-queens n)
  (accumulate (lambda (a b) (begin 
                              (print-board n a)
                              (newline)
                              b)) (newline) (queens n)))

(print-queens 4)