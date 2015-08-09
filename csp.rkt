#lang racket

; (define ns (make-base-namespace))

; Primitives
(define Coin 'coin)
(define Toffee 'toffee)
(define Choc 'chocolate)
(define Bleep 'bleep)
(define Around 'around)
(define Up 'up)
(define Down 'down)
(define End 'end)

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

(define (get-process process)
  (let ([arity (procedure-arity process)])
  (cond
    [(eq? arity 0) (process)]
    [(eq? arity 1) process])))
           
(define (interact-with-process alphabet process input)
  (let ([possible-events 
         (string-join
          (map symbol->string (menu alphabet (get-process process)))
          ", ")])
    (fprintf
     (current-output-port) "choose from the possible inputs: ~a~n" possible-events)
    (let ([i (read input)])
      (cond
        [(equal? (eval i) End)
         (displayln "reached the end, will loop again")
         (interact-with-process alphabet process input)]
        [(equal? ((get-process process) (eval i)) Bleep)
         (displayln Bleep)
         (interact-with-process alphabet process input)]
        [else (interact-with-process alphabet ((get-process process) (eval i)) input)]))))

(define (interact-with-process-batch alphabet process commands-list)
  (let ([possible-events (menu alphabet (get-process process))])
    (cons possible-events
          (cond
            [(empty? commands-list) empty]
            [(equal? (eval (car commands-list)) End) empty]
            [(equal? ((get-process process) (eval (car commands-list))) Bleep)
             (cons Bleep (interact-with-process-batch alphabet process (cdr commands-list)))]
            [else
             (interact-with-process-batch alphabet
                       ((get-process process)
                        (eval (car commands-list)))
                       (cdr commands-list))]))))

;; Sample processes
; The 'stop' process
(define (stop arg) Bleep)
(define (coin-then-stop x)
  (if (equal? x Coin)
      stop
      Bleep))

(define my-choice (choice2 Coin coin-then-stop Toffee stop))

; Example of a recursively defined process
(define VMS (prefix Coin (prefix Choc (lambda () VMS))))

; Example of mutually recursively defined process
(define CT
    (lambda (n)
        (if (equal? n 0)
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

;; (menu '(Coin Choc End) (VMS Coin))
;; (interact2 '(Coin Choc End) VMS (current-input-port))