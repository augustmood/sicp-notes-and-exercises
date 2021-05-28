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

(define (print-interval i)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")")
  (newline))

;; Interval A:
;; Center: x    Percentage: a   Lower-Bound: x * (1 - a)    Upper-Bound: x * (1 + a)
;; Interval B:
;; Center: y    Percentage: b   Lower-Bound: y * (1 - b)    Upper-Bound: y * (1 + b)

;; Product of A and B:
;; Lower-Bound: (x * (1 - a)) * (y * (1 - b)) = xy - bxy - axy + abxy
;; Upper-Bound: (x * (1 + a)) * (y * (1 + b)) = xy + bxy + axy + abxy
;; Center: xy + abxy = xy * (1 + ab)
;; Percentage: (axy + bxy) / (xy * (1 + ab)) = (xy * (a + b)) / (xy * (1 + ab)) = (a + b) / (1 + ab) 

(define (approx-mul-p x y)
  (let ((a (percent x))
        (b (percent y)))
    (/ (+ a b)
       (+ 1 (* a b)))))

;; test part:

(define (mul-per-verifier n)

  (define (mul-percentage-equal? x y)
    (= (approx-mul-p x y) (mul-percent x y)))
  
  (define (interval-generator n)
    (let ((center (random 1 n)))
      (let ((width (random 0 center)))
        (make-center-width center width))))
  
  (if (= n 1)
      (display "All tests pass!")
      (let ((x (interval-generator n))
            (y (interval-generator n)))
        (if (mul-percentage-equal? x y)
            (mul-per-verifier (- n 1))
            (begin 
              (display "Tests failed: ")
              (newline)
              (display "x: ")
              (print-interval x)
              (display "y: ")
              (print-interval y))))))

(mul-per-verifier 1000)