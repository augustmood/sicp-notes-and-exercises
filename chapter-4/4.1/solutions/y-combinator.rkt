#lang racket

(define length
  (lambda (lst)
    (cond [(null? lst) 0]
          [else (add1 (length (cdr lst)))])))

((lambda (length)
   (lambda (lst)
     (cond [(null? lst) 0]
           [else (add1 ((length length) (cdr lst)))])))
 (lambda (length)
   (lambda (lst)
     (cond [(null? lst) 0]
           [else (add1 ((length length) (cdr lst)))]))))

(lambda (f)
  ((lambda (u) (u u))
   (lambda (length)
     (f
      (lambda (v) ((length length) v))))))

; (lambda (g)
;   (lambda (lst)
;     (cond [(null? lst) 0]
;           [else (add1 (g (cdr lst)))])))

(define 
  y
  (lambda (f)
    ((lambda (u) (u u))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define fact
  (lambda (n)
    (cond [(= n 1) 1]
          [else (* n (fact (sub1 n)))])))

((y (lambda (g)
      (lambda (lst)
        (cond [(null? lst) 0]
              [else (add1 (g (cdr lst)))])))) '(1 2 3 4 5))

((y (lambda (fact)
      (lambda (n)
        (cond [(= n 1) 1]
              [else (* n (fact (sub1 n)))])))) 5)

(define fib
  (lambda (n)
    (cond [(<= n 2) 1]
          [else (+ (fib (- n 2)) (fib (- n 1)))])))

((y (lambda (fib)
      (lambda (n)
        (cond [(<= n 2) 1]
              [else (+ (fib (- n 2)) (fib (- n 1)))])))) 10)

; (define f
;   (lambda (x)
;     (define even?
;       (lambda (n)
;         (cond [(= n 0) true]
;               [else (odd? (- n 1))])))
;     (define odd?
;       (lambda (n)
;         (cond [(= n 0) false]
;               [else (even? (- n 1))])))
;     (even? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define even?
;   (lambda (n)
;     (cond [(= n 0) true]
;           [else (odd? (- n 1))])))

; (define odd?
;   (lambda (n)
;     (cond [(= n 0) false]
;           [else (even? (- n 1))])))

; (even? 10) #t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define even?
;   (lambda (n)
;     (cond [(= n 0) true]
;           [else (odd? (- n 1))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ((lambda (odd?) ;; even func
;    ((lambda (u) (u u))
;     (lambda (even?)
;       (lambda (n)
;         (cond [(= n 0) true]
;               [else ((odd? even?) (- n 1))])))))
 
;  (lambda (even?)  ;; odd func
;    (lambda (n)
;      (cond [(= n 0) false]
;            [else ((even? even?) (- n 1))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ((lambda (odd?) ;; even func
;    ((lambda (u) (u u))
;     (lambda (even?)
;       ((lambda (f)
;          (lambda (n)
;            (cond [(= n 0) true]
;                  [else (f (- n 1))])))
;        (odd? even?)))))

;  (lambda (even?)  ;; odd func
;    ((lambda (f)
;       (lambda (n)
;         (cond [(= n 0) false]
;               [else (f (- n 1))])))
;     (even? even?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (lambda (g)
;   (lambda (h)
;     ((lambda (x) ;; even func
;       ((lambda (u) (u u))
;        (lambda (y)
;          (g (x y)))))
;     (lambda (y)  ;; odd func
;       (h (lambda (n) ((y y) n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mutually-y
  (lambda (g)
    (lambda (h)
      ((lambda (x) ;; even func
         ((lambda (u) (u u))
          (lambda (y)
            (g (x y)))))
       (lambda (y)  ;; odd func
         (h (lambda (n) ((y y) n))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test:

(((mutually-y 
   (lambda (f)
     (lambda (n)
       (cond [(= n 0) true]
             [else (f (- n 1))]))))
  (lambda (f)
    (lambda (n)
      (cond [(= n 0) false]
            [else (f (- n 1))]))))
 10) ;; #t

(((mutually-y 
   (lambda (f)
     (lambda (n)
       (cond [(= n 0) true]
             [else (f (- n 1))]))))
  (lambda (f)
    (lambda (n)
      (cond [(= n 0) false]
            [else (f (- n 1))]))))
 11) ;; #f
