#lang sicp
(#%require "stream.rkt")
(#%require "ex3.70.rkt")

(define weight-fn-3 (lambda (i) (+ (expt (car i) 3) (expt (cadr i) 3))))
(define ramanujan-stream (weighted-pairs integers integers weight-fn-3))

; (define prev -inf.0)
; (define counter 0)
; (define (marking-duplicates i)
;   (if (= i prev)
;       (set! counter (+ counter 1))
;       (begin (set! prev i)
;              (set! counter 1)
;              (cons i counter)))
;   (cons i counter))

; (define ramanujan-numbers
;   (stream-map 
;    stream-car 
;    (stream-filter (lambda (i) (= (cdr i) 2))
;                   (stream-map 
;                    marking-duplicates 
;                    (stream-map weight-fn-3 ramanujan-stream)))))

;; or in this way:

(define (find-ramanujan seq weight)
  (define (ramanujan-iter seq prev-stored)
    (let ([scar (stream-car seq)]
          [scadr (stream-car (stream-cdr seq))])
      (let ([weighted-scar (weight scar)]
            [weighted-scadr (weight scadr)])
      (if (and (= weighted-scar weighted-scadr)
               (not (eq? weighted-scar prev-stored)))
          (cons-stream weighted-scar
                       (ramanujan-iter 
                        (stream-cdr (stream-cdr seq)) 
                        weighted-scar))
          (ramanujan-iter (stream-cdr seq) prev-stored)))))
  (ramanujan-iter seq 0))

(define ramanujan-numbers (find-ramanujan ramanujan-stream weight-fn-3))
(show-stream  ramanujan-numbers 6)

;; the next five are 4104; 13832; 20683; 32832; 39312
