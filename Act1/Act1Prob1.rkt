#lang racket

(define (foo x)
  (if  (even? (length x))
    (car x)
    (car (reverse x))
  )
)

(define (third x)
  (if (> (length x) 2)
    (list-ref x 2)
    '()
  )
)

(define (toList a b c)
  (cons a(cons b(cons c '())))
)

(define (palindrome x)
  (equal? (list->string(reverse(string->list x))) x)
)

(define (foo2 l i)
  (if (equal? i 0)
    (car(l))
    (foo2 (cdr(l)) (- i 1))
  )
)

(display (foo2('(1 2 3) 0)))
