; LECTURE 3B:

; “You solve a class of problems that are in the neighborhood of the problem you want to solve”
; How to use embedding of languages, the power of which come from procedures like define deriv:

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))


; Rules for finding the derivative of expressions that one might right in an algebraic context

; integrals and derivatives are opposites of each other, inverse operations, and they have the
; same rules. then why are integrals more difficult to calculate?
; rules moving to the right are reduction rules, smaller problems--a perfect direction for
; recursion to work. but going left, it doesn't work the same.
; and when I go left, I don't have any guarantee that the particular path with terminate.







