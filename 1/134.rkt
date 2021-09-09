#lang racket

(define (f g) (g 2))

(define square (lambda (x) (* x x)))

(f f)
