#lang racket

(define (make-interval a b)
  (cons a b))

(define (lower-bound itvl)
  (car itvl))

(define (upper-bound itvl)
  (cdr itvl))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
          (= (lower-bound y) 0))
      (display "error")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (revised-mul x y)
  (define (positive? i)
    (>= (lower-bound i) 0))
  (define (negative? i)
    (< (upper-bound i) 0))
  (define (case i)
    (cond ((positive? i) 1)
          ((negative? i) -1)
          (else 0)))
  (let ((case-x (case x))
        (case-y (case y))
        (xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (= case-x 1) (= case-y 1)) (make-interval (* xl yl) (* xu yu)))
          ((and (= case-x 1) (= case-y 0)) (make-interval (* xu yl) (* xu yu)))
          ((and (= case-x 1) (= case-y -1)) (make-interval (* xu yl) (* xl yu)))
          ((and (= case-x -1) (= case-y 1)) (make-interval (* xl yu) (* xu yl)))
          ((and (= case-x -1) (= case-y 0)) (make-interval (* xl yu) (* xl yl)))
          ((and (= case-x -1) (= case-y -1)) (make-interval (* xu yu) (* xl yl)))
          ((and (= case-x 0) (= case-y 1)) (make-interval (* xl yu) (* xu yu)))
          ((and (= case-x 0) (= case-y -1)) (make-interval (* yl xu) (* xl yl)))
          (else (make-interval (min (* xl yu) (* xu yl)) (max (* xl yl) (* xu yu)))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent center percentage)
  ;   (make-interval (- center (* center percentage)) (+ center (* center percentage))))
  (make-center-width center (* center percentage)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (width i) (center i)))

(define (mul-percent x y)
  (percent (revised-mul x y)))

(define (print-interval-c-p i)
  (display "(")
  (display (center i))
  (display ", ")
  (display (percent i))
  (display ")")
  (newline))

(define (print-interval i)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")")
  (newline))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define a (make-center-percent 10 0.5))
(define b (make-center-percent 20 0.1))

(print-interval-c-p (par1 a b))
(print-interval-c-p (par2 a b))
(newline)
(print-interval-c-p (div-interval a a))
;; (1.6666666666666667, 0.7999999999999999)
(print-interval-c-p (div-interval a b))
; (0.5303030303030303, 0.5714285714285714)

(newline)
(print-interval-c-p (par1 (div-interval a a) (div-interval a b)))
(print-interval-c-p (par2 (div-interval a a) (div-interval a b)))

;; Lem is right, by the definition mentioned in our codes above, our interval A is [5, 15],
;; A/A will be like: A * (one / A) reference to our definition, since 1/A is [1/15, 1/5], and 
;; we could get the result of A / A = A * (1 / A) = [1/3, 3] (1.6666666666666667, 0.7999999999999999),
;; While as the expection, a variable divided by itself should result in 1 instead of the result we 
;; get. Our interval B is [18, 22], A / B should be A * (1 / B) = [5, 15] * [1/22, 1/18] = [5/22, 5/6]
;; (0.5303030303030303, 0.5714285714285714). IMO, in the expression A/A, the funciton treats A in the 
;; denominator and A in the numerator as different vairables, (A is not fixed tho, but they should be 
;; the same regardless of where it is). Threfore, as we see in function par1, both R1 and R2 appear in
;; numerator and denominator, the different way the functions treated the same variables in different 
;; position leads to an error.