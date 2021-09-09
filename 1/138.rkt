#lang racket

(define (cont-frac-iter n d k)
  (define (iter a result)
    (println result)
    (if (= a 0)
      result
      (iter (sub1 a) (/ (n a) (+ (d a) result)))))
  (iter k 0))

(define (e-euler k)
  (+ 2 (cont-frac-iter (lambda (_) 1.0)
                       (lambda (i)
                         (if (= (remainder i 3) 2)
                           (/ (add1 i) 1.5)
                           1))
                       k)))

(e-euler 10)
