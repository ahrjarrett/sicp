; Define a procedure that takes 3 numbers as arguments and
; returns the sum of the squares of the two larger numbers.


(define (sum-of-largest-two a b c)
  (cond ((and (> a b) (> b c)) a + b)
        ((and (> a b) (> c b)) a + c)
        (else b + c)))

; Tests:
(sum-of-largest-two 1 2 3) ; 5
(sum-of-largest-two 6 5 4) ; 11
(sum-of-largest-two 7 9 8) ; 17
