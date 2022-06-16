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



