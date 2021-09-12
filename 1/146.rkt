#lang racket

(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess old)
      (if (good-enough? guess old)
        guess
        (iter (improve guess) guess)))
    (iter guess (add1 guess)))) ; old guess is arbitrary here

(define (nice-sqrt x)
  (define (good-enough? guess old)
    (< (abs (- guess old)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 3.0))
