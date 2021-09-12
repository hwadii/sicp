#lang racket

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define dx 0.00001)
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated-rec f n)
  (if (= n 0)
    identity
    (repeated-rec (compose f f) (sub1 n))))

(define (average a b) (/ (+ a b) 2))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (max-pow n)
  (define (iter p r)
    (if (> r n)
      (- p 1)
      (iter (+ p 1) (* r 2))))
  (iter 1 2))

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ y (expt x (- n 1)))) (repeated-rec average-damp (max-pow n)) 1.0))

(nth-root 10 2)
