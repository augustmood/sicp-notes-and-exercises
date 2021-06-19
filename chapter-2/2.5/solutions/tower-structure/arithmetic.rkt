#lang sicp
(#%require "interface.rkt")
(#%require "integer.rkt")
(#%require "rational-number.rkt")
(#%require "real.rkt")
(#%require "complex-number.rkt")
(#%require "polynomial.rkt")
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
  
  ; (define (complex->polynomial arg)
  ;   (if (= (imag-part arg) 0)
  ;       (if (and (eq? (type-tag (real-part arg)) 'polynomial)
  ;                (eq? (car (contents (real-part arg))) 'x))
  ;           (add (make-polynomial 'x '(sparse)) (real-part arg))
  ;           (make-polynomial 'x (list 'sparse (list 'term 0 (real-part arg)))))
  ;       (make-polynomial 'x (list 'sparse (list 'term 0 arg)))))
  
  (define (complex->polynomial arg)
    (make-polynomial 
     'x (adjoin-term 
         (make-term 0
                    (if (= (imag-part arg) 0)
                        (if (eq? (type-tag (real-part arg)) 'rational)
                            (rational->real (real-part arg))
                            (real-part arg))
                        arg)) '(sparse))))
  
  
  (define (polynomial->complex arg)
    (if (= (cadr (caddr (contents arg))) 0)
        (make-complex-from-real-imag (caddr (caddr (contents arg))) 0)
        (make-complex-from-real-imag 0 0)))
  ; (make-complex-from-real-imag arg 0)))))
  
  (define (complex->real arg)
    (let ((coered-arg (real-part arg)))
      (if (number? coered-arg) (exact->inexact coered-arg) coered-arg)))
  
  
  (define (real->rational arg)
    (define (iter arg denom)
      (if (integer? arg)
          (make-rational (inexact->exact arg) (inexact->exact denom))
          (iter (* 10 arg) (* 10 denom))))
    (iter arg 1))
  
  (define (rational->integer arg)
    (let ((contents (contents arg)))
      (inexact->exact (round (/ (car contents) (cdr contents))))))
  
  (define (polar->rect arg)
    (make-complex-from-real-imag (real-part arg) (imag-part arg)))
  
  (put-coercion 'integer 'rational integer->rational)
  (put-coercion 'rational 'real rational->real)
  (put-coercion 'real 'complex real->complex)
  (put-coercion 'complex 'polynomial complex->polynomial)
  (put-coercion 'rational 'integer rational->integer)
  (put-coercion 'real 'rational real->rational)
  (put-coercion 'complex 'real complex->real)
  (put-coercion 'polynomial 'complex polynomial->complex)
  (put-coercion 'polar 'rect polar->rect)
  )