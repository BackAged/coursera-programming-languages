#lang racket

(define (promisify fn)
  (mcons #f fn))

(define (await promise)
  (if (mcar promise)
      (mcdr promise)
      (begin (set-mcar! promise #t)
             (set-mcdr! promise ((mcdr promise)))
             (mcdr promise))))

(define (add x y)
  (lambda () (+ x y)))

(let ([promise (promisify (add 2 3))])
  (await promise))