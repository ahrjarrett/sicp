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

