#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

; (define (make-monitored f)
;   (define counter 0)
;   (define (mf m)
;     (cond ((number? m) (begin (set! counter (+ counter 1)) (f m)))
;           ((eq? m 'how-many-calls?) counter)
;           ((eq? m 'reset-count) (set! counter 0))
;           (else (error "Unknown request -- MAKE-MONITOREDF" m))))
;   mf)

(define (make-monitored f)
  (let ((counter 0))
    (lambda (m)
      (cond ((number? m) (begin (set! counter (+ counter 1)) (f m)))
            ((eq? m 'how-many-calls?) counter)
            ((eq? m 'reset-count) (set! counter 0))
            (else (error "Unknown request -- MAKE-MONITOREDF" m))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?) ;; 1
(s 100)
(s 'how-many-calls?) ;; 2
