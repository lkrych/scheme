(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; we haven't yet said how a rational number is represented or how make-rat
; numer or denom are implemented! we are simply wishfully thinking and using 
; well proven identities of rational numbers ;) 

; to facilitate with the creation of rational numbers we will use pairs which are constructed with cons

(define x (cons 1 2))
(car x) ;1
(cdr x) ;2
(define y (cons 3 4))
(define z (cons x y))
(car (car z)) ;1
(car (cdr z)) ;3

; this is all we need to define make-rat, numer and denom!
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
; redefine functions so that they can make use of our definitions
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))



(print-rat (add-rat one-half one-third)) ;5/6
(print-rat (mul-rat one-half one-third)) ;1/6
(print-rat (add-rat one-third one-third)) ;6/9
; note that our implementation does not reduce rational numbers to lowest terms.
; we can fix that by using gcd to reduce the numerator and denominator to loswest terms
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))


(define (make-rat n d)
  (let ((g (gcd n d)))
  (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third)); 2/3
; exercise 2.1 - define a better version of make-rat that handles both positive and negative arguments
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
  (cons (/ n g) (/ d g))))

; define a constructor and selector for defining a line segment
; define make-segment, start-segment, end-segment, make-point, x-point, y-point, midpoint-segment

(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-point x y) (cons x y))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (make-segment start end) (cons start end))

(define (midpoint-segment segment)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((a (start-segment segment))
	(b (end-segment segment)))
    (make-point (average (x-point a)
			 (x-point b))
		(average (y-point a)
			 (y-point b)))))
;test
(define seg (make-segment (make-point 2 3)
			  (make-point 10 15)))
(print-point (midpoint-segment seg))

;2.3 - implement a representation of rectangles in a plane

(define (make-rectangle bottom-left top-right) (cons bottom-left top-right))
(define (bottom-left rect) (car rect))
(define (top-right rect) (cdr rect))
(define (width rect)
  (- (car(top-right rect) (car (bottom-left rect)))))
(define (height rect)
  (- (cdr(top-right rect) (cdr (bottom-left rect)))))
(define (bottom-right rect)
  (make-point (x-point (cdr rect))
	      (y-point (car rect))))
(define (top-left rect)
  (make-point (x-point (car rect))
	      (y-point (cdr rect))))
(define (perimeter rect)
  (+ (* 2 (width rect))
     (* 2 (height rect))))
(define (area rect)
  (* (width rect)
     (height rect)))

;2.4 - here is an alternative procedure representation of pairs
; define cdr
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
; (cons 1 2) = (lambda (m) (m 1 2))
; (car(cons 1 2)) = lambda (lambda (p q) p) 1 2)
(car (cons 1 2))
; (cdr (cons 1 2)) = lambda (lambda (p q) q) 1 2)
(cdr (cons 1 2))

;2.5 - represent pairs of non-negative integers using only numbers and arithmetic operations if we represent the pair as the integer that is the product of 2^a*b^3

;this is an example of how weird we can make the implementation of certain primitives
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))
(define (largest-power-of a z)
  (if (= (remainder z a ) 0)
    (+ 1 (largest-power-of a (/ z a)))
    0))
(define (cons a b) (* (expt a 2) (expt b 3)))
(define (car z) (largest-power-of 2 z))
(define (cdr z) (largest-power-of 3 z))

;2.6 - todo

;section 2.1.4
;problem 2.7

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
		   (/ 1.0 (lower-bound y)))))

