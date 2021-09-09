#lang racket

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess counter)
    (let ([next (f guess)])
      (println (cons next counter))
      (if (close-enough? guess next)
        next
        (try next (add1 counter)))))
  (try first-guess 0))

(define (average a b) (/ (+ a b) 2))

(define (f x) (/ (log 1000) (log x)))
(define (f-damp x) (average x (f x)))

(fixed-point f 2.0)
(fixed-point f-damp 2.0)
