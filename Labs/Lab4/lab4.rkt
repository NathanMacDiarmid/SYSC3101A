#lang racket

;; SYSC 3101 A Winter 2023 Lab 4

;; Nathan MacDiarmid
;; 101098993

;; Exercise 1

(define (make-upcounter counter)
  (lambda () 
    (set! counter (+ counter 1))
    counter))


;; Exercise 2 

(define (make-counter counter)
  
  (define (count-up) 
    (set! counter (+ counter 1))
    counter)
  
  (define (count-down)
    (if (> counter 0)
        (begin (set! counter (- counter 1))
               counter)
        "Counter is 0"))

  (define (dispatch cmd)
    (cond ((eq? cmd 'inc) count-up)
          ((eq? cmd 'dec) count-down)
          (else (error "Unknown command:" cmd))))
  
  dispatch)


;; Exercise 3

(define (make-counter-with-let initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (dispatch cmd)
      (cond ((eq? cmd 'inc) count-up)
            ((eq? cmd 'dec) count-down)
            (else (error "Unknown command:" cmd))))
 
    dispatch))

;; Exercise 4

(define (make-counter-ex4 initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (lambda (command)
       (cond
          ((eq? command 'inc) count-up)
          ((eq? command 'dec) count-down)
          (else (error "Unknown command:" command))
          ))
    ))

;; Exercise 5

(define (make-counter-ex5 initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (get-count)
      counter
      )

    (define (reset-count)
      (set! counter 0)
      counter)

    (lambda (command)
       (cond
          ((eq? command 'inc) count-up)
          ((eq? command 'dec) count-down)
          ((eq? command 'get) get-count)
          ((eq? command 'reset) reset-count)
          (else (error "Unknown command:" command))
          ))
    ))

;; Exercise 6

(define (make-counter-ex6 initial-count increment)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter increment))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (get-count)
      counter
      )

    (define (reset-count)
      (set! counter 0)
      counter)

    (lambda (command)
       (cond
          ((eq? command 'inc) count-up)
          ((eq? command 'dec) count-down)
          ((eq? command 'get) get-count)
          ((eq? command 'reset) reset-count)
          (else (error "Unknown command:" command))
          ))
    ))

;; Exercise 7

(define (make-counter-ex7 initial-count increment)

  (let ((counter initial-count)
        (high-water-mark 0))
 
    (define (count-up)
      (set! counter (+ counter increment))
      (cond
        ((> counter high-water-mark) (set! high-water-mark counter)))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (get-count)
      counter
      )

    (define (get-max)
      high-water-mark)

    (define (reset-count)
      (set! counter 0)
      (set! high-water-mark 0)
      counter)

    (lambda (command)
       (cond
          ((eq? command 'inc) count-up)
          ((eq? command 'dec) count-down)
          ((eq? command 'get) get-count)
          ((eq? command 'max) get-max)
          ((eq? command 'reset) reset-count)
          (else (error "Unknown command:" command))
          ))
    ))
