;1.14
;draw the tree illustrating the process generated by:
;count-change 11
; (count-change 11)
; (cc 11 5)
; (+ (cc 11 4) (cc -39 5))
; (+ (+ (cc 11 3) (cc -14 4) (0))
; (+ (+ (+ (cc 11 2) (cc 1 5) (0) (0))))
; (+ (+ (+ (+ (cc 11 1) (+ (cc -49 5) (cc 1 4)) (0) (0)))))
; (+ (+ (+ (+ (+ (cc 11 0) (cc 10 1)) (+ (cc -49) (cc 1 4)) (0) (0)))))

; (count-change 11)
; |
; (cc 11 5)__
; |          \
; (cc 11 4)   (cc -39 5)
; |       \___
; |           \
; (cc 11 3)   (cc -14 4)
; |       \_______________________________________________________
; |                                                               \
; (cc 11 2)                                                      (cc 1 3)
; |       \_________________________                              |     \__
; |                                 \                             |        \
; (cc 11 1)                        (cc 6 2)                      (cc 1 2) (cc -9 3)
; |       \___                      |     \__                     |     \__
; |           \                     |        \                    |        \
; (cc 11 0)   (cc 10 1)            (cc 6 1) (cc 1 2)             (cc 1 1) (cc -4 2)
;          __/ |                 __/ |       |     \__            |     \__
;         /    |                /    |       |        \           |        \
; (cc 10 0)   (cc 9 1)  (cc 6 0)   (cc 5 1) (cc 1 1) (cc -4 2)   (cc 1 0) (cc 0 1)
;          __/ |                 __/ |       |     \__
;         /    |                /    |       |        \
; (cc 9 0)    (cc 8 1)  (cc 5 0)   (cc 4 1) (cc 1 0) (cc 0 1)
;          __/ |                 __/ |
;         /    |                /    |
; (cc 8 0)    (cc 7 1)  (cc 4 0)   (cc 3 1)
;          __/ |                 __/ |
;         /    |                /    |
; (cc 7 0)    (cc 6 1)  (cc 3 0)   (cc 2 1)
;          __/ |                 __/ |
;         /    |                /    |
; (cc 6 0)    (cc 5 1)  (cc 2 0)   (cc 1 1)
;          __/ |                 __/ |
;         /    |                /    |
; (cc 5 0)    (cc 4 1)  (cc 1 0)   (cc 0 1)
;          __/ |
;         /    |
; (cc 4 0)    (cc 3 1)
;          __/ |
;         /    |
; (cc 3 0)    (cc 2 1)
;          __/ |
;         /    |
; (cc 2 0)    (cc 1 1)
;          __/ |
;         /    |
; (cc 1 0)    (cc 0 1)


; what are the orders of growth of the space and number of steps used by this process as the amount to be changed increases?
; the space complexity  consumed by the recursive process is going to be proportional to the maximum height of the recursion tree. 
; space complexity is going to be linear
; time complexity is exponential O(n^k), where k is the number of coins used 
; https://cs.stackexchange.com/a/68704

;1.15 
(define (cube x) (* x x x))
(define (p x) 
    (newline) (display "calling p ") (display x)
    (- (* 3 x) (* 4 (cube x)))) 
(define (sine angle)
    (if (not (> (abs angle) 0.1)) 
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)
; how many times is the procedure p applied when (sine 12.15) is evaluated
; ans -> 5
; what is the order of growth in space and number of steps (as a function of a) used by the process generated by the procedure when
; sine a is evaluated?
; ans -> it is logarithmic because the value a is divided by 3 each time