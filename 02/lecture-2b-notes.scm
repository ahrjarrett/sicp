; LECTURE 2B: COMPOUND DATA


;; PART 1:

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


;; PART 3:

; a good analogue to MAKE-RAT might be MAKE-POINT,
; where the car and cdr represent x and y coordinates
; (or vectors in the plane):
(define (make-vector x y) (cons x y))
(define (xcor p) (car p))
(define (xcor p) (cdr p))

; and maybe we want to build a SEGMENT in terms of vectors,
; make up of two points, p and q:
(define (make-seg p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

; and what if we wanted the MIDPOINT of a segment s?
(define (midpoint s)
  (let ((a (seg-start s))
        (b (seg-end s)))
    (make-vector
      (average (xcor a) (xcor b))
      (average (ycor a) (ycor b)))))
      ; basically the let statement is destructuring the segment right?

; and then if we wanted to compute the length of the segment:
; Using Pythagorean Theorem: √(dx * dx + dy * dy)
(define (length s)
  (let
   ((dx (- (xcor (seg-end s))
           (xcor (seg-start s))))
    (dy (- (ycor (seg-end s))
           (ycor (seg-start s)))))
   (sqrt (+ (square dx)
           (square dy)))))

; as above, here is the abstraction barrier:

; SEGMENTS
; ===============================
;   make-seg/seg-start/seg-end     <-- “abstraction layer”
; ===============================
; VECTORS
; =========================
;   make-vector/xcor/ycor   <-- “abstraction layer”
; =========================
; PAIRS

; the idea of CLOSURE is very important here,
; i.e. a pair of a pair is closed over by
; the pair context above
; (notice the slightly different definition
; of closure...or is it really different?)


;; PART 4:
; this part is going to be harder, where we’ll be talking about
; “what it means when” we have these data abstractions

; for example, returning to the axion for pairs:
(car (cons x y)) ; => x
(cdr (cons x y)) ; => y

; but we’re never told what a pair REALLY is.

; “pairs can be built out of nothing at all” <-- huh?

; for example, if we were to implement cons, car & cdr:
; we have a procedure that returns a procedure
(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))
(define (car x) (x 1))
(define (cdr x) (x 2))

; notice that there are no data objects in any of these
; procedures. I’m creating them out of thin air.
; all I have to do is fulfill the axiom.

; READ THAT AGAIN, SLOWLY.

; this is where we start blurring the lines
; between what is data, and what is a procedure.
