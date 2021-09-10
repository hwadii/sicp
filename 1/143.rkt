#lang racket

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g k)
    (if (= k 0)
      g
      (iter (compose f g) (sub1 k))))
  (iter identity n))

(define (repeated-rec f n)
  (if (= n 0)
    identity
    (repeated-rec (compose f f) (sub1 n))))
