#lang racket

(define (quicksort lst)
	(if (null? lst)
		'()
		(append
			(quicksort (filter (lambda (num) (< num (car lst))) (cdr lst)))
			(list (car lst)) (filter (lambda (num) (= num (car lst))) (cdr lst))
			(quicksort (filter (lambda (num) (> num (car lst))) (cdr lst)))
		)
	)
)

(define (transpose matrix)
	(cond
		((null? (car matrix)) null)
		(else (cons (map car matrix) (transpose (map cdr matrix))))
	)
)

(define (multMatrices m1 mat2)
	(define m2 (transpose mat2))
	(map (lambda (lst1) (map (lambda (lst2) (apply + (map (lambda (n1 n2) (* n1 n2)) lst1 lst2))) m2)) m1)
)

(define (getAverages table)
  (map
    (lambda (gender)
      (list
        gender
        ((lambda (filtered)
          (exact->inexact (/ (apply + (map cadr filtered)) (length filtered)))
        )
        (filter (lambda(x) (eq? (car x) gender)) table))
      )
    ) '(male female)
  )
)

(define (insert x bst)
	(if (empty? bst)
		(list x '() '())
		(if (< x (car bst))
			(list (car bst) (insert x (cadr bst)) (caddr bst))
			(list (car bst) (cadr bst) (insert x (caddr bst)))
		)
	)
)

(define (hasPath graph n1 n2)
  (not (empty? (filter
    (lambda (g) (and (eq? (car g) n1) (eq? (cadr g) n2)))
    graph
  )))
)


(getAverages '((male 178) (male 188) (female 167) (female 169) (male 178)))
