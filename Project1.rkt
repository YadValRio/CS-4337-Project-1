#lang racket

(displayln "Enter a postfix/Polish notation expression:")

(define history null)

(define notation null)

(define (pop stack) ; usable for either history or notation
  (if (null? stack)
     (error "The stack you are trying to access is empty")
     (values (car stack) (cdr stack))))

(define (push stack) ; also applicable for both locations
  (cons item stack))