#lang racket

((lambda (x y) (+ x y)) 5 6)


(define (filterP l)
  (filter
    (lambda (li) (= (remainder li 7) 0))
    l
  )
)

(define (sumAll l)
  (if (null? l)
    0
    (+ (sumAll (cdr l)) (car l))
  )
)

(define (sumAll2 l)
  (apply + l)
)

(define (edgeExist graph pair)
    (not (empty? (filter
        (lambda (g) (equal? g pair))
        graph
      )
    ))
)

(define (matrixAddition matA matB)
  (map (lambda (a b) (map + a b)) matA matB)
)

(define (balance lst)
  (if (empty? lst)
    0
    (if (eq? (caar lst) "in")
      (+ (caddar lst) (balance (cdr lst)))
      (+ (- (caddar lst))  (balance (cdr lst)))
    )
  )
)

(edgeExist '((A B) (B A) (C A) (C D)) '(C A))
