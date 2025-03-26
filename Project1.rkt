#lang racket

; Stacks and their functions

(define history null)

(define nums null)

(define operations null)

(define (pop stack) ; usable for either nums or notation
  (if (null? stack)
      (error "The stack you are trying to access is empty") 
      (values (car stack) (cdr stack))))

(define (push stack item) ; also usable for both all stacks
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
         (set! operations (push operations char))]
        [else (displayln("Invalid input detected. Must be either a number or a simple operation ('+', '-', '*', '/')."))]))))

(define (solve-equation)
  (if (and (not (null? nums)) (not (null? operations)))
      (let-values ([(num1 new-nums) (pop nums)])
        (let-values ([(num2 final-nums) (pop new-nums)])
          (let-values ([(op new-operations) (pop operations)])
            (define result (calculate num2 num1 op))
            (set! nums (push final-nums result))
            (set! operations new-operations)
            (solve-equation))))  ; Recursive call
      (void)))  ; Base case: Do nothing when stacks are empty

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
(solve-equation)

(displayln nums)

(displayln operations)