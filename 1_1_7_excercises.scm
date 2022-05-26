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

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (new-sqrt-iter guess x)
  ;new-if if you want to test 1.1.6
  (if (good-enough? guess x)
    guess
    (new-sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (new-sqrt-iter 1.0 x))

;1.1.6 - what happens when we use new-if to compute square roots?
; answer -> an infinite loop, when the new-if is invoked, it is a procedure call, this means
; that all of the arguments are evaluated. Because one of the arguments is a recursive call
; the predicate is never reached, and the program loops forever just evaluating the args.
(sqrt 9)

;1.1.7 examples of implementation failing with very small and very large numbers
(sqrt 0.00002)
; 0.00447
(sqrt 1000000000000)
;316227.7
; why is the good-enough? function inadequate for small and large numbers?
; the good-enough? function relies on the heuristic of checking to see whether the absolute 
; value between the square of a guess and the value is less than 0.001. For smaller values
; the tolerance of 0.001 is large when computing the square root of a small value. It isn't
; precise enough to settle on a viable answer. For very large values, the machine precision
; is unable to represent the differences between large numbers. The Algorithm might never terminate; because the square of the best guess will not be within 0.001 of the radicand and trying to 
; improve will yield the same guess.

; improvement that stops when change is a very small fraction of guess
(define (good-enough-better? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))

(define (sqrt-better guess x)
  ;new-if if you want to test 1.1.6
  (if (good-enough-better? guess x)
    guess
    (sqrt-better (improve guess x) x)))

(define (sqrtb x)
  (sqrt-better 1.0 x))

(sqrtb 0.00002)
; 0.00447
(sqrtb 1000000000000)
;1000000

;1.1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

; another variant that only stops when improvement stops
(define (good-enough-best? prev_guess guess)
  (< (abs (/ (- prev_guess guess) guess)) 0.0001))

(define (cube-iter prev_guess guess x)
(if (good-enough-best? prev_guess guess)
  guess
  (cube-iter guess (improve-cube guess x) x)))

(define (3root x)
  (cube-iter 0.0 1.0 x))

(3root 27)
(sqrt 16)

