(define (square x) (* x x))
(define (abs x)
  (if (< x 0) (- x) x))

;a guess is good enough if the square of guess is close to x
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 16)
(sqrt 21)

; localize the subprocedures so that folks using sqrt don't have to worry about it
; use lexical-scoping to capture value of x
(define (sqrt2 x)
  (define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
    guess
    (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt2 9)
(sqrt2 16)
(sqrt2 21)