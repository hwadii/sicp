#lang racket

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond [(> a b) result]
          [(filter a) (iter (next a) (combiner result (term a)))]
          [else (iter (next a) result)]))
  (iter a null-value))

(define (sum-square-primes a b)
  #| Suppose that we have a procedure to know if a number is a prime |#
  (define (prime? x) x)
  (filtered-accumulate + prime? 0 sqr a add1 b))

(define (product-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * relative-prime? 1 identity 1 add1 n))


(sum-square-primes 1 3)

(product-primes 10)
