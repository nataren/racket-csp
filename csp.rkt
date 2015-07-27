#lang racket

; Primitives
(define Coin 'coin)
(define Toffee 'toffee)
(define Choc 'chocolate)
(define Bleep 'bleep)
(define Around 'around)
(define Up 'up)
(define Down 'down)

; The 'prefix' notation
(define (prefix event process)
  (lambda (e)
    (if (equal? e event)
        process
        Bleep)))

; The 'choice' (aka '|') notation
(define (choice2 e1 p1 e2 p2)
  (lambda (e)
    (cond
      [(equal? e e1) p1]
      [(equal? e e2) p2]
      [else Bleep])))

; The 'menu' function returns the events that can appear as
; the first event for the given process and and the given alphabet
(define (menu alphabet process)
  (cond 
    [(empty? alphabet) empty]
    [(equal? (process (eval (car alphabet))) Bleep) (menu (cdr alphabet) process)]
    [else (cons (car alphabet) (menu (cdr alphabet) process))]))

; Sample processes

; The 'stop' process
(define (stop arg) Bleep)
(define (coin-then-stop x)
  (if (equal? x Coin)
      stop
      Bleep))

(define my-choice (choice2 Coin coin-then-stop Toffee stop))

; Example of a recursively defined process
(define VM (prefix Coin (prefix Choc (lambda () VM))))

; Instance of a recursively defined process
(define coin-then-choc (((VM Coin) Choc)))

; Example of mutually recursively defined process
(define CT
    (lambda (n)
        (if (= n 0)
            (choice2 Around (lambda () (CT 0))
                     Up (lambda () (CT 1)))
            (choice2 Up (lambda () (CT (+ n 1)))
                     Down (lambda () (CT (- n 1)))))))

; Instance of a mutually recursively defined process
(define ct_0 (CT 0))
(define ct_0_around ((ct_0 Around)))
(define ct_1 ((ct_0 Up)))
(define ct_2 ((ct_1 Up)))
(define around-upup-downdown-down ((((((((((((CT 0) Around)) Up)) Up)) Down)) Down)) Down))

