#lang racket

; Stacks and their functions

(define history null)

(define nums null)

(define (pop stack)
  (if (null? stack)
      (error "The stack you are trying to access is empty") 
      (values (car stack) (cdr stack))))

(define (push stack item)
  (cons item stack))

; Calculator

(define (get-notation)
  (display "Enter a postfix/Polish notation expression (or type \"quit\" to quit: ")
  (define input (read-line))
  (unless (equal? input "quit")
    (for ([char (string->list input)])
      (cond
        ; if is a number
        [(char-numeric? char)
         (set! nums (push nums (string->number (string char))))]
        ; if is an operation
        [(member char '(#\+ #\- #\* #\/))
         (let-values ([(num1 new-nums) (pop nums)])
           (let-values ([(num2 final-nums) (pop new-nums)])
             (define result (calculate num1 num2 char))
             (set! nums (push final-nums result))))]
        [else (displayln("Invalid input detected. Must be either a number or a simple operation ('+', '-', '*', '/')."))]))
  (displayln nums) ; if done correctly, nums should only have the final answer at the end
  (set! nums null)
  (get-notation)))

(define (calculate num1 num2 operation)
  (define result
    (cond
      [(char=? operation #\+) (+ num1 num2)]
      [(char=? operation #\-) (- num1 num2)]
      [(char=? operation #\*) (* num1 num2)]
      [(char=? operation #\/) (if (= num2 0)
                                  (error "Division by 0 is not allowed!")
                                  (/ num1 num2))]
      [else (error "Invalid operators! Only '+', '-', '*', '/' are allowed!")]))
  (set! history (push history result))
  result)

; Run all together

(get-notation)
