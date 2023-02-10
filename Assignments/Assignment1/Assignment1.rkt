#lang racket

; Assignment 1
; Nathan MacDiarmid
; 101098993

; Question 1

(define (count-multiples lst n)
  (cond
    [(empty? lst) + 0]
    [(< n 1) + 0]
    [(= (modulo (car lst) n) 0) (+ 1 (count-multiples (cdr lst) n))]
    [(count-multiples (cdr lst) n)]
   )
  )

; Question 2

(define (count-multiples-iter lst n)
  (for/sum ([i lst])
    (cond
      [(< n 1) 0]
      [(= (modulo i n) 0) 1]
      [0]
      )
    )
  )

; Question 3

(define (deep-list-remove test-procedure lst)
  (cond
    [(empty? lst) `()]
    ; Checks if element after first element is start of another list
    [(cons? (car lst))
     ; Sets the beginning of next list portion as same list as previous element
     (cons (deep-list-remove test-procedure (car lst)) (deep-list-remove test-procedure (cdr lst)))]
    ; Checks if the first element meets the lambda expression
    [(test-procedure (car lst)) (deep-list-remove test-procedure (cdr lst))]
    ; Constructs the new list
    [(cons (car lst) (deep-list-remove test-procedure (cdr lst)))]
    )
  )