#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (cond
    [(> low high) null]
    [else (cons low (sequence (+ low stride) high stride))]))


(define (string-append-map xs suffix)
  (map (lambda (el) (string-append el suffix)) xs))

(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(empty? xs) (error "list-nth-mod: empty list")]
    [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (cond
    [(or (zero? n) (negative? n)) null]
    [else
     (let ([p (s)])
       (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))])) 

(define funny-number-stream
  (letrec ([fdn (lambda (x) (if (zero? (modulo x 5)) (* -1 x) x ))]
           [f (lambda (x) (cons (fdn x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


(define dan-then-dog
  (letrec ([fdn (lambda (x) (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg"))]
           [f (lambda (x) (cons x (lambda () (f (fdn x)))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (s) (lambda () (cons (cons 0 (car (s))) (f (cdr (s))))))])
         (f s)))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (lambda () (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (f (+ x 1)))))])
    (f 0)))

(define (vector-assoc v vec)
  (let ([len (vector-length vec)])
    (letrec ([f (lambda (pos)
      (cond ((= pos len) #f)
            ((pair? (vector-ref vec pos))
                    (if (equal? (car (vector-ref vec pos)) v)
                        (vector-ref vec pos)
                        (f (+ pos 1))))
            (#t (f (+ pos 1)))))])
      (f 0))))