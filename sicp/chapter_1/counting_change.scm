(define (count_change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount
                (- kinds-of-coins 1))
            (cc (- amount
                (first-denomination kinds-of-coins))
                kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count_change 100)

; this function is a tree-recursive process with redundancies like fib and it is not easy to
; see how to create a better algorithm 
; one approach to coping with redundant computations is to arrange it so that the algorithm
; maintains a table of values as they are computed. Each time we are asked to apply the procedure
; to some argument, we first look to see if the value is already stored in the table. This strategy is known
; as memoization