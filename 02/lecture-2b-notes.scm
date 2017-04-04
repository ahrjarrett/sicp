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
; (where gcd means greatest common denominator)
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

; we’d still need to write +RAT, *RAT, -RAT, etc. etc.
; the important thing is that we’ve created an abstraction layer
; btwn these procedures and the pairs that they operate upon:

; +RAT *RAT -RAT ... ...
; =========================
;   make-rat/numer/denom     <-- “abstraction layer”
; =========================
; PAIRS

; this is called “DATA ABSTRACTION”:
; we’re separating the USE of data objects
; from the REPRESENTATION of data objects

; defining +RAT without data abstraction:
(define (+rat x y)
  (cons (+ (* (car x) (cdr y))
           (* (car y) (cdr x)))
        (* (cdr x) (cdr y))))

; so why even use data abstraction?
; -> goes back to the idea that if you have
;    a name for something, you have control of it

; nowhere in +rat above do we have anything that
; points to a rational numbers as a conceptual entity

; so now we can rewrite numer and denom in terms of
; each other and their greatest common denominator:
(define (make-rat n d) (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
       (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
       (/ (cdr x) g)))

; different btwn DEFINE and LET:
; define will, from definition on, bind a value to another one
; let will create a local definition

; BOOKMARK: 40:47 on 04/04/17
