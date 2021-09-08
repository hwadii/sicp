#lang racket

(define (cube x) (expt x 3))

 (define (sum term a next b)
   (if (> a b)
     0
     (+ (term a)
        (sum term (next a) next b))))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simpson-term k)
    (* (cond [(or (= k 0) (= k n)) 1]
           [(even? k) 2]
           [else 4]) (y k)))
  (* (/ h 3.0) (sum simpson-term 0 add1 n)))

(simpson-rule cube 0 1 1000)
