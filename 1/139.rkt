#lang racket

(define (cont-frac-iter n d k)
  (define (iter a result)
    (if (= a 0)
      result
      (iter (sub1 a) (/ (n a) (+ (d a) result)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (sqr x))))
                  (lambda (i) (sub1 (* 2 i)))
                  k))
