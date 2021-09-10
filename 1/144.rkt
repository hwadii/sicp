#lang racket

(define dx 0.00001)
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated-rec f n)
  (if (= n 0)
    identity
    (repeated-rec (compose f f) (sub1 n))))

(define (average a b c) (/ (+ a b c) 3.0))

(define (smooth f)
  (λ (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-smooth f n)
  ((repeated-rec smooth n) f))

((n-smooth sqr 100) 10)
