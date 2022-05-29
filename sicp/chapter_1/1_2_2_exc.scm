;1.11
; n if n<3,
;f(n−1)+2f(n−2)+3f(n−3) if n≥3
; write a procedure that computes f by means of recursion
(define (f n)
    (cond ((< n 3) n)
        (else (+ (f (- n 1))
            (* 2 (f (- n 2)))
            (* 3 (f (- n 3)))))))
(f 1) ;1
(f 2) ;2
(f 3) ;4
(f 4) ;11
(f 5) ;25
(f 6) ;59
(f 7) ;142

; difference between values
; 1 2 7 14 24 83
;write a procedure that computes f by means of an iterative process
;(f 5)
; f(4) + 2*f(3) + 3*f(2))
; f(4) = f (3) + (2 * f (2)) + (3* f(1)) 
(define (f-better n)
    (define (f-iter a b c count)
        (cond ((< n 3) n)
        ((<= count 0) a)
        (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
    (f-iter 2 1 0 (- n 2)))

(f-better 1) ;1
(f-better 2) ;2
(f-better 3) ;4
(f-better 4) ;11
(f-better 5) ;25
(f-better 6) ;59
(f-better 7) ;142