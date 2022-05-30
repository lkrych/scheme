;1.16 design an iterative exponention process that uses successive squaring
(define (even? n)
    (= (remainder n 2 ) 0))

(define (fast-expt b n)
    (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

;comparing the two designs you can see that we added a new accumulator value A
; we have also moved the square in line 7 into the recursive call
; we have also moved the * b in line 8 into the recursive call

(define (iter-fast-expt b n)
    (define (expt-iter N B A)
        (cond ((= 0 N) A)
            ((even? N) (expt-iter (/ N 2) (square B) A))
            (else (expt-iter (- N 1) B (* B A)))))
    (expt-iter n b 1))

(iter-fast-expt 2 4)
;(iter-fast-expt 2 4)
;(expt-iter 4 2 1)
;(expt-iter 2 4 1)
;(expt-iter 1 16 1)
;(expt-iter 0 16 16)
;16
(iter-fast-expt 2 5)
;(iter-fast-expt 2 5)
;(expt-iter 5 2 1)
;(expt-iter 4 2 2)
;(expt-iter 2 4 2)
;(expt-iter 1 16 2)
;(expt-iter 0 16 32)

(iter-fast-expt 2 7)
;(expt-iter 7 2 1)
;(expt-iter 6 2 2)
;(expt-iter 3 4 2)
;(expt-iter 2 4 8)
;(expt-iter 1 16 8)
;(expt-iter 0 16 128)
