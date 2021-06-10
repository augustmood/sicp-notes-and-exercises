#lang sicp
(#%require "interface.rkt")

;; a

(define (install-division-a)
  (define (get-record e-name files)
    (cond ((null? files) nil)
          ((equal? e-name (key (car files)) (cons (cadr files) (get-record e-name (cdr files)))))
          (else (get-record e-name (cdr files)))))

  (define (get-salary e-name record) ;; (cadadr '(a (adress salary)))
    (cadadr record))
  
  (put 'get-record 'a get-record)
  (put 'get-salary 'a get-salary)
  'done)

(define (get-record e-name files)
  ((get 'get-record (type-tag files))) e-name files)

(define (get-salary e-name record)
  ((get 'get-salary (type-tag record))) e-name record)

(define (find-employee-record e-name lod) ;; list of divisions' files
  (if (null? lod)
      nil
      (cons (get-record e-name (car lod)) 
            (find-employee-record e-name (cdr lod)))))

;; d.

;; install the new divison package and add the files into the lists.