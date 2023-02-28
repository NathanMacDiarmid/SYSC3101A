#lang racket

; Assignment 1
; Nathan MacDiarmid
; 101098993

; Question 1

(define (count-multiples lst n)
  (cond
    ; Verifies the list is not null
    [(empty? lst) + 0]
    ; Verifies that n is greater than 1 as per requirements
    [(< n 1) + 0]
    ; Adds one to the total sum if a multiple is found
    [(= (modulo (car lst) n) 0) (+ 1 (count-multiples (cdr lst) n))]
    ; Goes to next element in list
    [(count-multiples (cdr lst) n)]
   )
  )

; Question 2

(define (count-multiples-iter lst n)
  ; for/sum adds the result of all the iterations
  (for/sum ([i lst])
    (cond
      [(< n 1) 0]
      ; Verifies that the element in the list is a multiple of n
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