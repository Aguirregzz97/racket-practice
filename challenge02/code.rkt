#lang racket

(define (isPrime2 n num)
  (if (< n (sqrt num))
    (if (equal? (modulo num n) 0)
      #f
      (isPrime2 (+ n 1) num)
    )
    #t
  )
)

(define (isPrime n)
  (if (equal? n 1)
    #f
    (isPrime2 2 n)
  )
)


(define (sumDigits l)
  (if (null? l)
    0
    (+ (sumDigits (cdr l)) (+ (exact-floor (log (car l) 10)) 1))
  )
)

(define (sumLists l1 l2)
  (if (null? l1)
    l2
    (if (null? l2)
      l1
      (if (< (car l1) (car l2))
        (cons (car l1) (sumLists (cdr l1) l2))
        (cons (car l2) (sumLists (cdr l2) l1))
      )
    )
  )
)

(define (boolMerge l1 l2)
  (map bitwise-ior l1 l2)
)

(define (listToNumber2 l sum)
  (if (null? l)
    sum
    (listToNumber2 (cdr l) (+ (* sum 10) (car l)))
  )
)

(define (listToNumber l)
  (listToNumber2 l 0)
)

(define (getList g)
  (if (null? g)
    '()
    (append (getList (cdr g)) (car g))
  )
)

(define (getUnique g)
  (remove-duplicates (getList g))
)

(define in?
    (lambda (x S)
      (cond ((empty? S) #f)
            ((equal? (car S) x) #t)
            (else (in? x (cdr S)))
      )))

(define equal-sets?
  (lambda (A B)
    (andmap (lambda (x) (in? x B)) A)
  )
)

(define (isComplete g)
  (if (null? g)
    #f
    (equal-sets? (car g) (cadr g))
  )
)

(define (sumMatrix m)
  (if (null? m)
    0
    (+ (sumMatrix (cdr m)) (apply + (car m)))
  )
)

(isComplete '((a b c) (b a) (c)))
