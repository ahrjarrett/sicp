; LECTURE 2B: COMPOUND DATA

; (cons x y)
; constructs a pair where the first part is x, 2nd part is y
; (car p)
; selects the 1st part of the pair p
; (cdr p)
; selects the 2nd part of the pair p

; From the Clojure Docs:
; (cons x seq)
; “Returns a new seq where x is the first element and seq is the rest.”

(define (make-rat n d)
  (cons n d))

(define (numer x) (car x))
(define (demon x) (cdr x))

(define a (make-rat 1 2))
(define b (make-rat 1 4))

; problem with this implementation is that it doesn’t reduce
; the rational number to its lowest possible terms
; (e.g. a + b returns 6/8 instead of 3/4)

; a better implementation would be:
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))
