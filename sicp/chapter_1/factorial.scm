(define (factorial x)
  (if (= x 1)
    1
    (* x (factorial(- x 1)))))

;24
(factorial 4)
;120
(factorial 5)
