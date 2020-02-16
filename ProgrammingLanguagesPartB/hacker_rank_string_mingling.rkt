#lang racket

(define (print-mingled-string a b)
  (for ([i (in-string a)]
        [j (in-string b)])
    (display i)(display j)))
  


(let ([a (read-line)]
      [b (read-line)])
  (print-mingled-string a b))
  