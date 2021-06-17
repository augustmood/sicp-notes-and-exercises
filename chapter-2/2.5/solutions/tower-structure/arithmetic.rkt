#lang sicp
(#%require "interface.rkt")
(#%require "integer.rkt")
(#%require "rational-number.rkt")
(#%require "real.rkt")
(#%require "complex-number.rkt")
(#%require "operators.rkt")
(#%provide (all-defined))

(define (install-arithmetic)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package))

(define (install-coercion)
  (define (integer->rational arg)
    (make-rational arg 1))
  
  (define (rational->real arg)
    (let ((contents (contents arg)))
      (exact->inexact (/ (car contents) (cdr contents)))))
  
  (define (real->complex arg)
    (make-complex-from-real-imag (exact->inexact arg) 0))
  
  (define (rational->integer arg)
    (let ((contents (contents arg)))
      (inexact->exact (round (/ (car contents) (cdr contents))))))
  
  (define (real->rational arg)
    (define (iter arg denom)
      (if (integer? arg)
          (make-rational (inexact->exact arg) (inexact->exact denom))
           (iter (* 10 arg) (* 10 denom))))
    (iter arg 1))
  
  (define (complex->real arg)
    (if (number? arg) (exact->inexact (real-part arg)) (real-part arg)))
  
  (define (polar->rect arg)
    (make-complex-from-real-imag (real-part arg) (imag-part arg)))
  
  (put-coercion 'integer 'rational integer->rational)
  (put-coercion 'rational 'real rational->real)
  (put-coercion 'real 'complex real->complex)
  (put-coercion 'rational 'integer rational->integer)
  (put-coercion 'real 'rational real->rational)
  (put-coercion 'complex 'real complex->real)
  (put-coercion 'polar 'rect polar->rect)
  )