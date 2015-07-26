#lang racket

; Primitives
(define Coin 'coin)
(define Toffee 'toffee)
(define Choc 'chocolate)
(define Bleep 'bleep)
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