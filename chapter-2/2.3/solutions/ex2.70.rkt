#lang racket
(require "huffman.rkt"
         "ex2.68.rkt"
         "ex2.69.rkt")

(define rock-pair '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-pair))
(define rock-song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah 
                        yip yip yip yip yip yip yip yip yip Sha boom))

(define encoded-song 
  (encode (map string->symbol 
               (map string-upcase 
                    (map symbol->string rock-song))) rock-tree))

;; (length encoded-song) <- 84 bits required for the encoding
;; If we used a fixed-length code for the eight-symbol alphabet, the smallest number of bits that 
;; would be needed to encode this song is (* 3 (length rock-song)) <- 108.