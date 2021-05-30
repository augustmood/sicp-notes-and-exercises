#lang sicp

(define (square i)
    (* i i))

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things) 
;               (cons (square (car things))
;                     answer))))
;   (iter items nil))

;; In the procedure above, for each time the answer gets updated, the new squared item is added in 
;; front of the old answer -> (cons (square (car (things))) answer)

; Louis's edition:

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square (car things))))))
;   (iter items nil))

;; Louis's code is incorrec since it adds the old answer list to the new list with new squared item at
;; each iteration to a new item.



; My version is correct:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items nil))

(square-list (list 1 2 3 4))