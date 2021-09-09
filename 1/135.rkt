#lang racket

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (println next)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))

(define golden-ratio
  (fixed-point (lambda (y) (add1 (/ 1 y))) 1.0))

#| Examples |#

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

