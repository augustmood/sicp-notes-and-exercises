#lang racket

;; She is right, as we've already dicussed in the solution to exercise 2.14, division handles the same
;; variables when they appaer in different positions (numerator and denominator). In function par1, 
;; the non-fixed variables are computed repeatedly, while in par 2, they are counted directly only 
;; once, so par2 is a better program for parallel resistances.