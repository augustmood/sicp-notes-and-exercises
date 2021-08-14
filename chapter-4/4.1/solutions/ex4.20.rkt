#lang sicp

;; a

; (define (make-let parameters body)
;   (cons 'let (cons parameters body)))

;; '(let* <bindings> <body>)

; (define (letrec? exp) (tagged-list? exp 'letrec))
; (define (binding-var bind) (car bind))
; (define (binding-val bind) (cdr bind))
; (define (letrec-bindings exp) (cadr exp))
; (define (letrec-body exp) (cddr exp))
; (define (binding->set bind) (cons 'set (binding-var bind) (binding-val bind)))
; (define (letrec->let exp)
;   (let ([bindings (letrec-bindings exp)])
;     (make-let 
;      (map 
;       (lambda (i) 
;         (cons i '*unassigned*))
;       (map binding-var bindings))
;      (list->exp (append (map binding->set bindings) (letrec-body exp))))))

;; add this line to eval:

; (eval (letrec->let exp) env)

;; b

;; it won't work, since if just simply using `let`, the function cannot be 
;; properly evaluated.
