#lang racket

; EXERCISE 1

(define (sum-coins pennies nickels dimes quarters)
  (+ (* pennies 1) (* nickels 5)
     (* dimes 10) (* quarters 25))
  )

; EXERCISE 2

(define (interest balance)
  (cond
    [(or (< balance 1000) (= balance 1000)) (exact-floor (* balance 0.04))]
    [(or (< balance 5000) (= balance 5000)) (exact-floor (* balance 0.045))]
    [(exact-floor (* balance 0.05))])
  )

; EXERCISE 3

(define (balance bank-balance)
  (+ (interest bank-balance) bank-balance)
  )

; EXERCISE 4

(define (variable_Interest balance)
  (cond
    [(> balance 5000) (exact-floor (+ (* (- balance 5000) 0.05) (* 4000 0.045) (* 1000 0.04)))]
    [(> balance 1000) (exact-floor (+ (* (- balance 1000) 0.045) (* 1000 0.04)))]
    [(exact-floor (* balance 0.04))]
    )
  )