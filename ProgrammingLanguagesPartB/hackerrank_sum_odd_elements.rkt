#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (read-list)
  (let ([r (read)])
    (if (eof-object? r) empty
        (cons r (read-list)))))

(define (inverse-list lst)
    (cond
    [(empty? (cdr lst)) (list (car lst))]
    [else (append (inverse-list (cdr lst)) (list (car lst)))]))

(let* ([ls (read-list)]
    [inv-lst (foldr  (1 2 3 4))])
    ( (printf "~a\n" x)))