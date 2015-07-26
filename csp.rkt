#lang racket

; Primitives
(define Coin 'coin)
(define Toffee 'toffee)
(define Choc 'chocolate)
(define Bleep 'bleep)
(define Around 'around)
(define Up 'up)
(define Down 'down)

(define (stop arg) Bleep)

(define (prefix event process)
  (lambda (e)
    (if (equal? e event)
        process
        Bleep)))

(define (choice2 e1 p1 e2 p2)
  (lambda (e)
    (cond
      [(equal? e e1) p1]
      [(equal? e e2) p2]
      [else Bleep])))

; Sample processes
(define (coin-then-stop x)
  (if (equal? x Coin)
      stop
      Bleep))

(define my-choice (choice2 Coin coin-then-stop Toffee stop))

; Example of a recursively defined process
(define VM
  (lambda ()
    (prefix Coin (prefix Choc VM))))

; Instance of a recursively defined process
(define coin-then-choc (((VM) Coin) Choc))


; Example of mutually recursively defined process
(define CT
    (lambda (n)
        (if (= n 0)
            (choice2 Around (lambda () (CT 0))
                     Up (lambda () (CT 1)))
            (choice2 Up (lambda () (CT (+ n 1)))
                     Down (lambda () (CT (- n 1)))))))

; Instance of a mutually recursively defined process
(define around-upup-downdown-down ((((((((((((CT 0) Around)) Up)) Up)) Down)) Down)) Down))

