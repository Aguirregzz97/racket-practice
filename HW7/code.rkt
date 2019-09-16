#lang racket

(define (elementExist li item)
  (not (empty? (filter (lambda (lis) (eq? lis item)) li)))
)

(define (getIndexAux li curr val len)
  (if (= curr len)
    -1
  (if (eq? (car li) val)
    curr
    (getIndexAux (cdr li) (+ curr 1) val len)
  )
  )
)

(define (getIndex li val)
  (getIndexAux li 0 val (length li))
)

(define (exist? el li)
  (not (empty? (filter (lambda (lis) (= el lis)) li)))
)

(define (allDifferent? li)
	(if (null? li)
		#t
		(if (exist? (car li) (cdr li))
			#f
			(allDifferent? (cdr li))
		)
	)
)

(allDifferent? '(1 2 2 3))
