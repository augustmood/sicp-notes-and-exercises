#lang racket
(require test-engine/racket-tests)
(require racket/trace)

;; Euclid's Algorithm:
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(trace gcd)
(gcd 206 40)

(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0)
        40
        (gcd (remainder 206 40) 
            (remainder 40 (remainder 206 40))))

(if (= 6 0) ;; remainer 1
    40
    (gcd (remainder 206 40) 
            (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) 
            (remainder 40 (remainder 206 40)))

(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd 
        (remainder 40 (remainder 206 40))
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40)))))

(if (= (remainder 40 6) 0) ;; remainder 2
    (remainder 206 40)
    (gcd 
        (remainder 40 (remainder 206 40))
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40)))))

(if (= 4 0) ;; remainder 3
    (remainder 206 40)
    (gcd 
        (remainder 40 (remainder 206 40))
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40)))))

(gcd 
    (remainder 40 (remainder 206 40))
    (remainder 
        (remainder 206 40)
        (remainder 40 (remainder 206 40))))

(if (= (remainder 
        (remainder 206 40)
        (remainder 40 (remainder 206 40))) 0)

    (remainder 40 (remainder 206 40))

    (gcd 
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; (gcd b

        (remainder 
            (remainder 40 (remainder 206 40)) ;; (remainder a
            
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))))) ;; b)

(if (= (remainder 
        (remainder 206 40)
        (remainder 40 6)) 0) ;; remainder 4

    (remainder 40 (remainder 206 40))
    
    (gcd 
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; (gcd b

        (remainder 
            (remainder 40 (remainder 206 40)) ;; (remainder a
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))))) ;; b)

(if (= (remainder 
        (remainder 206 40)
        4) 0) ;; remainder 5

    (remainder 40 (remainder 206 40))
    
    (gcd 
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; (gcd b

        (remainder 
            (remainder 40 (remainder 206 40)) ;; (remainder a
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))))) ;; b)

(if (= (remainder 6 4) 0) ;; remainder 6
    (remainder 40 (remainder 206 40))

    (gcd 
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; (gcd b

        (remainder 
            (remainder 40 (remainder 206 40)) ;; (remainder a
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))))) ;; b)

(if (= 2 0) ;; remainder 7
    (remainder 40 (remainder 206 40))

    (gcd 
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; (gcd b

        (remainder 
            (remainder 40 (remainder 206 40)) ;; (remainder a
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))))) ;; b)

(gcd 
    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (remainder 
        (remainder 40 (remainder 206 40)) ;;
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))))) ;; b

(if (= (remainder 
        (remainder 40 (remainder 206 40))
        (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40)))) 0) ;; predicate

    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= (remainder 
        (remainder 40 (remainder 206 40))
        (remainder 
            (remainder 206 40)
            (remainder 40 6))) 0) ;; predicate ;;remainder 8

    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= (remainder 
        (remainder 40 (remainder 206 40))
        (remainder 
            (remainder 206 40)
            4)) 0) ;; predicate ;;remainder 9

    ((remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40)))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= (remainder 
        (remainder 40 (remainder 206 40))
        (remainder 
            6
            4)) 0) ;; predicate ;;remainder 10

    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= (remainder 
        (remainder 40 (remainder 206 40)) 2) 0) ;; predicate ;;remainder 11

    ((remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40)))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= (remainder 
        (remainder 40 6) 2) 0) ;; predicate ;;remainder 12

    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= (remainder 4 2) 0) ;; predicate ;;remainder 13

    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(if (= 0 0) ;; predicate ;;remainder 14

    (remainder 
            (remainder 206 40)
            (remainder 40 (remainder 206 40))) ;; a

    (gcd 
        (remainder 
            (remainder 40 (remainder 206 40))
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; (gcd b

        (remainder 
            (remainder 
                (remainder 206 40)
                (remainder 40 (remainder 206 40)))
            (remainder 
                (remainder 40 (remainder 206 40))
                (remainder 
                    (remainder 206 40)
                    (remainder 40 (remainder 206 40))))))) ;; (remainder a b))

(remainder 
    (remainder 206 40)
    (remainder 40 (remainder 206 40))) ;; a

(remainder 
    (remainder 206 40)
    (remainder 40 6)) ;; remainder 15

(remainder (remainder 206 40) 4) ;; remainder 16

(remainder  6 4) ;; remainder 17

2 ;; remainder 18


;; In normal-order:
; there're 18 remainder operations are actually performed in the normal-order evaluation of 
; (gcd 206 40)


;; In applicative order:
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40))) 

(gcd 40 (remainder 206 40)) ;; remainder 1

(gcd 40 6)

(if (= 6 0)
        40
        (gcd 6 (remainder 40 6)))

(gcd 6 (remainder 40 6)) ;; remainder 2

(gcd 6 4)

(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

(gcd 4 (remainder 6 4)) ;; remainder 3

(gcd 4 2)

(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

(gcd 2 (remainder 4 2)) ;; remainder 4

(gcd 2 0)

(if (= 0 0)
    2
    (gcd 2 (remainder 2 0)))

2

;; In Appicative-order:
; there're 4 remainder operations are actually performed in the applicative-order evaluation of 
; (gcd 206 40)