#lang racket

(define (reverse l)
  (if (null? l)
    l
    (append (reverse (cdr l)) (list (car l)))
  )
)

(define (sum l)
  (if (null? l)
    0
    (if (list? (car l))
      (sum (cdr l))
      (+ (sum (cdr l)) (car l))
    )
  )
)

(define (sum2 l)
  (if (list? l)
    (if (empty? l)
      0
      (+ (sum2 (car l)) (sum2 (cdr l)))
    )
    l
  )
)

(define (nested l)
  (if (list? l)
    (if (empty? l)
      '()
      (append (nested (car l)) (nested (cdr l)))
    )
    (list l)
  )
)

(define (mean l)
  (exact->inexact(/ (sum l) (length l)))
)

(define (fibonacci x)
  (if (equal? x 0)
    0
    (if (equal? x 1)
      1
      (+ (fibonacci (- x 1)) (fibonacci (- x 2)))
    )
  )
)
