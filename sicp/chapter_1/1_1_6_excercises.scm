;1.1
10 ;10
(+  5 3 4) ;12
(- 9 1) ;8
(/ 6 2) ;3
(+ (* 2 4) (- 4 6)) ;-16
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) ;19
(= a b) ;f
(if (and (> b a) (< b (* a b))) b a) ;b

(cond ((= a 4) 6) ;f
      ((= b 4) (+ 6 7 a)) ;t
      (else 25));16

( + 2 (if (< b a) b a)) ;5

( * (cond ((> a b) a)
	  ((< a b) b)
	  (else -1))
    (+ a 1)) ;16

;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))(* 3 (- 6 2) (- 2 7)))

;1.3
(define (square x) (* x x))
(define (sum_squares x y) (+ (square x) (square y))) 
(define (sum_squares_two_largest x y z)
  (cond ((and (> y x) (> z x)) (sum_squares y z))
	((and (> x y) (> z y)) (sum_squares x z))
	((and (> x z) (> y z)) (sum_squares x y))
	))
(sum_squares_two_largest 1 2 3) ;13
(sum_squares_two_largest 2 1 3) ;13
(sum_squares_two_largest 3 2 1) ;13

;1.4 what does the following expression do?
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 2) ;3
(a-plus-abs-b 1 -2) ;3
; we can conditionally return an expression for evaluation depending on the value of b
;1.5
;(define (p) (p))
;(define (test x y)
;  (if (= x 0) 0 y))

;(test 0 (p))

; what behavior will the expression yield with applicative-order evaluation? i.e evaluate subexpression, apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments
; Ans: we get into an infinite loop because p evaluates as itself, which in turn needs to be evaluated 
; what behavior will the expression yield with normal-order evaluation? i.e. fully expand and reduce
; Ans: we receive 0 because p is never evaluated


