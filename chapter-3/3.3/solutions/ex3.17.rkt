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

(define (count-pairs x)
  (let ((distinct-pairs (cons '() '())))
    (define (count x)
      (cond [(and (pair? x) (and (memq x distinct-pairs) #t)) distinct-pairs]
            [(pair? x) (begin (append! distinct-pairs (list x))
                              (count (car x))
                              (count (cdr x)))]
            [else (append! distinct-pairs '())]))
    (length (cdr (count x)))))

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
(count-pairs return-4) ;; 3
return-7
(count-pairs return-7) ;; 3
never-return
(count-pairs never-return) ;; 3
