#lang racket

(define (product-rec term a next b)
 (if (> a b)
   1
   (* (term a)
      (product term (next a) next b))))

(define (factorial a)
  (product identity 1 add1 a))

(define (pi-approx n)
  (define (pi-term n)
    (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
  (* 4.0 (product-rec pi-term 1 add1 n)))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-approx-formula n)
  (define (term i)
    (+ 1 (/ (- (* 4 (sqr i)) 1))))
  (exact->inexact (product-rec term 1 add1 n)))
