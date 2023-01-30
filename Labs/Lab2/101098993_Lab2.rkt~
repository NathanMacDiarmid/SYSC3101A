#lang racket

; Exercise 1
; Part a)

(define (sum-numbers numbers)
  (cond
   [(empty? numbers) + 0]
   [(+ (car numbers) (sum-numbers (cdr numbers)))]
   ))

; Part b)

(define (average numbers)
  (exact->inexact(/ (sum-numbers numbers) (length numbers)))
  )

; Exercise 2

(define (occurrences numbers n)
  (cond
    [(empty? numbers) + 0]
    [(= (car numbers) n) (+ 1 (occurrences (cdr numbers) n))]
    [(occurrences (cdr numbers) n)]
    )
  )

; Exercise 3

(define (convert digits)
  (car (cdr digits))
  )

; Exercise 4

; Exercise 5