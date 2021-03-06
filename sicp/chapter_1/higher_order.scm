; compute the sum of integers from a through b
(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

; compute the sum of cubes of integers from a through b
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a)
       (sum-cubes (+ a 1) b))))

;compute sum of a sequence 1/1*3 + 1/5*7 + 1/9*11...
(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

;the three procedures above share a common underlying pattern
; they are for the most part identical, differing only in the name of the procedure, the 
; function used to compute the term to be added, and the function to provides the next
; value of a

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))


; redefine sum-cubes using higher order func
(define (inc n) (+ n 1))
(define (sum-cubes2 a b)
  (sum cube a inc b))

(sum-cubes 1 10)
(sum-cubes2 1 10)

; redefine sum-integers using higher order func
(define (identity x) x)
(define (sum-integers2 a b)
  (sum identity a inc b))

(sum-integers 1 10)
(sum-integers2 1 10)

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
(* 8 (pi-sum2 1 1000))

; define integral of function f between limits a and b
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; the exect value of the integral of the cube between 0 and 1 is 1/4
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

