;; LECTURE 4B -- GENERIC OPERATORS


;; PART I - Text Section 2.3

;; Generic operator means, what it precisely does depends
;; on what kind of data it is looking at (reminds me of
;; the seq interface/abstraction in Clojure).

;; Let's make some generic operators for dealing with
;; complex numbers:

;; Here are the Selectors we need to write:
;; (real-part z)
;; (imag-part z)
;; (magnitude z)
;; (angle z)

;; Here are the Constructors:
;; (make-rectangular x y)
;; (make-polar r a)

(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))


;; So now we need to write the operations:
(define (make-rectangular x y)
  (cons x y))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (make-polar r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (magnitude z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle z)
  (atan (cdr z) (car z)))


(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))


;; type predicates:
(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))


;; Rectangular package
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (real-part-rectangular z)
  (car z))

(define (imag-part-rectangular z)
  (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle-rectangular z)
  (atan (cdr z) (car z)))

(define (make-polar r a)
  (attach-type 'polar (cons r a)))

(define (real-part-polar z)
  (* (car z) (cos (cdr z))))

(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular
          (contents z)))
        ((polar? z)
         (real-part-polar
          (contents z)))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular
          (contents z)))
        ((polar? z)
         (imag-part-polar
          (contents z)))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular
          (contents z)))
        ((polar? z)
         (angle-polar
          (contents z)))))






















