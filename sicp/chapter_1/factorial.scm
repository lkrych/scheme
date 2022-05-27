(define (factorial x)
  (if (= x 1)
    1
    (* x (factorial(- x 1)))))

;24
(factorial 4)
;120
(factorial 5)

(define (factorial-iter x)
  (define (fact-iter product counter)
    (if (> counter x)
    product
    (fact-iter (* counter product)
                (+ counter 1))))
  (fact-iter 1 1))

;24
(factorial-iter 4)
;120
(factorial-iter 5)