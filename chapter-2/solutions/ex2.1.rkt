#lang racket
(require test-engine/racket-tests)
; Define a better version of make-rat that handles both positive and negative arguments. Make-rat 
; should normalize the sign so that if the rational number is positive, both the numerator and 
; denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (n (if (> (* n d) 0)
               (abs n)
               (- (abs n))))
        (d (abs d)))
    (cons (/ n g) (/ d g))))


;; test part

(print-rat (make-rat -3 9))
(print-rat (make-rat 3 -9))
(print-rat (make-rat -3 -9))

(check-expect (make-rat -3 9) '(-1 . 3))
(check-expect (make-rat 3 -9) '(-1 . 3))
(check-expect (make-rat -3 -9) '(1 . 3))
(test)