; this definition of expt is linear recursive, which requires O(n) steps
; O(n) space

(define (expt b n)
    (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(expt 2 4)
(expt 2 5)


; this linear iterative version of expt requires O(n) steps and O(1) space.
(define (expt2 b n)
    (expt-iter b n 1))
(define (expt-iter b counter product)
    (if (= counter 0)
    product
    (expt-iter b
        (- counter 1)
        (* b product))))

(expt2 2 4)
(expt2 2 5)

; we can compute exponentials in fewer steps by using successive squaring
; b^4 = b*b*b*b
; b^2 = b * b
; b^4 = b^2 * b^2
; 3 versus 4 multiplications


; fast-expt grows logarithmically with n in both space and number of steps
(define (even? n)
    (= (remainder n 2 ) 0))

(define (fast-expt b n)
    (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

(fast-expt 2 4)
(fast-expt 2 5)