#lang racket

(define Φ .6180)
(define ϵ .0001)
(define (close-enough? a) (< (abs (- a Φ)) ϵ))

(define (cont-frac n d k)
  (if (= k 0)
    0
    (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter a result)
    (cond [(= a k) (begin (begin (printf "~a in ~a iterations~n" result a) result))]
          [(close-enough? result) (begin (printf "~a in ~a iterations~n" result a) result)]
          [else (iter (add1 a) (/ (n k) (+ (d k) result)))]))
  (iter 0 0))

(/ (cont-frac-iter (lambda (_) 1.0)
                   (lambda (_) 1.0)
                   100))
