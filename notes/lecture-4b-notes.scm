;; LECTURE 4B -- GENERIC OPERATORS


;; PART I - Text Section 2.3

;; Generic operator means, what it precisely does depends
;; on what kind of data it is looking at (reminds me of
;; the seq interface/abstraction in Clojure).

;; Let's make some generic operators for dealing with
;; complex numbers:

(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

;; Bookmark ~ 5/6 min in



