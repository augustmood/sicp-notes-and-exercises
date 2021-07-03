#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

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
return-3
(count-pairs return-3) ;; 3
return-4
(count-pairs return-4) ;; 4
return-7
(count-pairs return-7) ;; 7
never-return
; (count-pairs never-return) ;; never return
