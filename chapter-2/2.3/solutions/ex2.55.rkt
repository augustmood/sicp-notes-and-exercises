#lang sicp

; Eva Lu Ator types to the interpreter the expression
(car ''abracadabra)
; To her surprise, the interpreter prints back quote. Explain.


;; ''abracadabra is the same as (quote (quote abracadabra)), thus (car ''abracadabra) is equal to
(car (quote (quote abracadabra)))
;; meanwhile, (quote (quote abracadabra)) is being evaluating as
(list 'quote '(quote abracadabra))
;; Thus, `(car ''abracadabra)` will produce quote