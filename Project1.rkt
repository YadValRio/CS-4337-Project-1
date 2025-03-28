#lang racket

(define prompt?
  (let [(args (current-command-line-arguments))]
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

; Stacks and their functions

(define history null)

(define nums null)

(define (pop stack)
  (if (null? stack)
      (error "Error: the stack you are trying to access is empty") 
      (values (car stack) (cdr stack))))

(define (push stack item)
  (cons item stack))

; Calculator

(define (get-notation)
  (with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)) (get-notation))]) ; will start over get-notation whenever an error is encountered
  (unless (not prompt?)
    (display "Enter a prefix/Polish notation expression (or type \"quit\" to quit): "))
  (define input (read-line))
  (unless (equal? input "quit")
    (for ([char (reverse(string->list input))])
      (cond
        ; if is a number
        [(char-numeric? char)
         (set! nums (push nums (real->double-flonum (string->number (string char)))))]
        ; if is '$', pop the last number from the stack, find that index in the history, and push back onto the stack
        [(char=? char #\$)
         (if (null? nums)
             (displayln "Error: no number to find the index of")
             (let-values ([(index new-nums) (pop nums)])
               (if (or (not (real? index)) (< index 1) (> index (length history)))
                   (error "Error: Invalid index for history extraction")
                   (let ([retrieved (get-value history index)])
                     (set! nums (push new-nums retrieved))))))]   
        ; if is an operation
        [(member char '(#\+ #\- #\* #\/))
         (let-values ([(num1 new-nums) (pop nums)])
           (let-values ([(num2 final-nums) (pop new-nums)])
             (define result (calculate num1 num2 char))
             (set! nums (push final-nums result))))]
        ; ignore whitespace
        [(char-whitespace? char)]
        [else error ("Error: invalid input detected. Please enter either a number or a simple operation ('+', '-', '*', '/').")]))
    (add-to-history nums)
    (get-notation))))

(define (calculate num1 num2 operation)
  (define result
    (cond
      [(char=? operation #\+) (+ num1 num2)]
      [(char=? operation #\-) (- num1 num2)]
      [(char=? operation #\*) (* num1 num2)]
      [(char=? operation #\/) (if (= num2 0)
                                  (error "Error: division by 0 is not allowed!")
                                  (/ num1 num2))]
      [else (error "Error: invalid operators! Only '+', '-', '*', '/' are allowed!")]))
  result)

(define (add-to-history stack)
  (when (not (null? nums))
    (let-values ([(result new-nums) (pop nums)])
      (set! history (push history result))
      (display #\$)
      (display (length history))
      (display ": ")
      (displayln result)
      (set! nums null))))

(define (get-value lst index)
  (define reversed-lst (reverse lst)) 
  (let ([int-index (exact-floor index)])
    (if (or (< int-index 1) (> int-index (length reversed-lst)))
        (error "Error: Index out of bounds")
        (list-ref reversed-lst (- int-index 1)))))

; Run all together

(get-notation)
