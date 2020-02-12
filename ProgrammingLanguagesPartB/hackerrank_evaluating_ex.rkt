#lang racket

(define (lazy-pow x)
  (let* ([res 1])
    (lambda () (begin (set! res (* res x)) res))))

(define (lazy-factorial)
  (let* ([st 0]
         [acc 1])
    (lambda () (begin (set! st (+ st 1))(set! acc (* acc st)) acc))))

(define (ex pow-stream factorial-stream to)
  (if (= to 1) 1.0000
      (+ (/ (pow-stream) (factorial-stream)) (ex pow-stream factorial-stream (- to 1))))) 

(define (ans x)
  (let ([pow (lazy-pow x)]
        [factorial (lazy-factorial)])
     (ex pow factorial 10)))


(define (read-list)
  (let ([r (read)])
    (if (eof-object? r) null
        (cons r (read-list)))))

(let ([n (read)]
      [ls (read-list)])
  (for ([x ls])
    (printf "~a\n" (ans x))))



  