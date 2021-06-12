#lang sicp
(#%require "interface.rkt")
(#%require "complex-numbers.rkt")

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  ;; modifications: Add
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;; modifications ends.

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; (define z (make-complex-from-real-imag 3 4))
; z
; (magnitude z)

;; It is much easier to show why by adding `display` in apply-generic to display those args and ops.

; (define (apply-generic op . args)
;   (display "start")
;   (newline)
;   (display "op: ")
;   (display op)
;   (newline)
;   (display "args: ")
;   (display args)
;   (newline)
;   (let ((type-tags (map type-tag args)))
;     (display type-tags)
;     (newline)
;     (display args)
;     (newline)
;     (display "end")
;     (newline)
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;            "No method for these types -- APPLY-GENERIC"
;            (list op type-tags))))))

;; proc <- (get 'magnitude '(complex)) <- magnitude
;;      (magnitude (map contents args)) <- (magnitude (cdr (complex rectangular 3 . 4))))
;;                                  <- (magnitude (rectangular 3 . 4)) <- 5

;; it is much easier to show why by adding `display` in apply-generic to display those args and ops.


;; Since we generally tag complex numbers as "complex" in our complex package, thus when we call 
;; `magnitude`, when it goes into `apply-generic`, the local variable type-tags will only return the 
;; symbol `complex`, which does not have a corresponding method, so `apply-generic` will 
;; return an error. What Louis did is just assign the procedure `magnitude` to the tag '(complex) as
;; its corresponding method, and in this case, the magnitude will go deep further to reach the 
;; more "precise" tag `rectangular` or `polar` instead of the general one `complex` and returned the 
;; expected value.

;; In conclusion, the `apply-generic` is invoked twice, and the first time, the procedure dispatched 
;; is `magnitude` in `complex`, and the second time, the one is also `magnitude` but for `rectangular`
;; or `polar` (in our rectangular case above is for `rectangualr` of course).