#lang racket

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (cube a) (* a a a))

(define (sum-cubes a b)
  (sum cube a add1 b))

(sum-cubes 1 3)
