#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (cycle? x)
  (define (helper x)
    (if (pair? x)
        (if (and (not (pair? (car x)))
                 (eq? (car x) -inf.0))
            #t
            (helper (cdr x)))
        #f))
  (if (pair? x) 
      (set-car! x -inf.0) 
      x)
  (helper (cdr x)))

(define return-3 (list 'a 'b 'c))
(define return-4 (let ((temp (list 'a 'b 'c)))
                   (begin (set-car! temp (cddr temp))
                          temp)))
(define return-7 (let ((temp (list 'a 'b 'c)))
                   (begin (set-car! temp (cdr temp))
                          (set-car! (cdr temp) (cddr temp))
                          temp)))
(define never-return (let ((temp (list 'a 'b 'c)))
                       (begin (set-cdr! (cddr temp) temp)
                              temp)))
(cycle? return-3) ;; f
(cycle? return-4) ;; f
(cycle? return-7) ;; f
(cycle? never-return) ;; t

;; O(1) Space and O(n) time
