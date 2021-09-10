#lang racket

(define (double g) (λ (x) (g (g x))))

(((double (double double)) add1) 5)
