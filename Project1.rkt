#lang racket

; Stacks and their functions

(define history null)

(define notation null)

(define (pop stack) ; usable for either history or notation
  (if (null? stack)
     (error "The stack you are trying to access is empty")
     (values (car stack) (cdr stack))))

(define (push stack item)
  (cons item stack))

(define (push-char-stack stack str) ; pushes the input string as a list of chars
  (foldl push stack (string->list str)))

; Calculator

(define (get-notation)
  (display "Enter a postfix/Polish notation expression (or type \"quit\" to quit: ")
  (define input (read-line))
  (unless (equal? input "quit")
    (set! notation (push-char-stack notation input))))

(define (calculate num1 num2 operation)
  (cond
    [(char=? operation #\+) (+ num1 num2)]
    [(char=? operation #\-) (- num1 num2)]
    [(char=? operation #\*) (* num1 num2)]
    [(char=? operation #\/) (if (=num2 0)
                         (error "Division by 0 is not allowed!")
                         (/ num1 num2))]
    [else (error "Invalid operators! Only '+', '-', '*', '/' are allowed!")]))

; Run all together

(get-notation)

(displayln notation)