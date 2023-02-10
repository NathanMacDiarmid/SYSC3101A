#lang racket

; Assignment 1
; Nathan MacDiarmid
; 101098993

; Question 1

(define (count-multiples lst n)
  (cond
   [(empty? lst) + 0]
   [(= (modulo (car lst) n) 0) (+ 1 (count-multiples (cdr lst) n))]
   [(count-multiples (cdr lst) n)]
   )
  )

; Question 2

(define (count-multiples-iter lst n)
  (for/sum ([i lst])
    (cond
      [(= (modulo i n) 0) 1]
      [0]
      )
    )
  )

; Question 3