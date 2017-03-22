; TODO: Need to define or import square
; BOOKMARK: 03/21/17 stopped at 22:30, halfway through lecture

(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (1+ a) b))))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-sq (1+ a) b))))

(define (square x) x * x)

;(sum-int 2 5) ; 14
(sum-sq 5 8) ; 176

; NOTES:
; Anytime we have almost identical things like sum-int and sum-sq,
; (or Leibnitz’ formula for computing PI over a)
; we're going to have to come up with some sort of ABSTRACTION
; to cover them.

; usually you learn a number of idioms when learning a computer
; language, common patterns of usage (gives the example of FORTRAN
; problem of finding largest number in a set)

; What we're doing instead is giving a name to that type of knowledge
; The pattern is:

(define (<name> a b)
  (if (> a b)
      0
      (+ (<term> a)
         (<name> (<next> a) b))))

; Numbers are just one kind of data.
; Procedures are simply a name we’ve given to a data
; that behaves a particular way

; So as an abstraction:


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next ; kinda confused about next here (see 12:00 - 14:00 min)
              b))))

; weird how we define it inside of sum-int, IMO
; why don't we put identity a inside parens?
; and how does this do the same thing as sum-int?

(define (sum-int-2 a b)
  (define (identity a) a)
  (sum identity a 1+ b))

; Ohh now I get it. We defined sum, so sum-int-2
; is simply figuring out which args to pass to the
; SUM function (the trickiest part seems to be
; figuring out what the term is; in the case of sum-int-2,
; the term is the IDENTITY function

(define (sum-sq-2 a b)
  (sum square a 1+ b))


; (LAMBDA) : first example of an anonymous function!
(define (pi-sum a b)
  (sum (LAMBDA (i) (/ 1 (* i (+ i 2))))
       a
       (LAMBDA (i) (+ i 4))
       b))


; here’s an iterative interpretation of SUM
; sometimes might be better (but seems to fly
; in the face of the whole LISP philosophy
(define (sum-iter term a next)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a ))



