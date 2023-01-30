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
  (cond
    [(empty? digits) 0]
    [(+ (car digits) (* (convert (cdr digits)) 10))]
    )
  )

; Exercise 4

(define (convertFC temps)
  (cond
    [(empty? temps)  `()]
    [(cons (convertFCFormula(car temps)) (convertFC (cdr temps)))]
    )
  )

(define (convertFCFormula temp)
  (* (- temp 32) 5/9)
  )

; Exercise 5

(define (eliminate-threshold numbers threshold)
  (cond
    [(empty? numbers)  null]
    [(<= (car numbers) threshold) (cons (car numbers) (eliminate-threshold (cdr numbers) threshold))]
    [(eliminate-threshold (cdr numbers) threshold)]
    )
  )