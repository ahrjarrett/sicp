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

; Integrals and derivatives are opposites of each other, inverse operations, and they have the
; same rules. then why are integrals more difficult to calculate?
; rules moving to the right are reduction rules, smaller problems--a perfect direction for
; recursion to work. but going left, it doesn't work the same.
; and when I go left, I don't have any guarantee that any particular path with terminate.

; below: a1 and a2 refer to
; WATCH VIDEO AGAIN: 10:00 to 13:00 to get this correct...
; also, is this function also called deriv?
(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (deriv (m1 exp) var)
                        (m2 exp))))))


; Procedures are list structure, where the operator is the CAR
; and the operands are the successive CARs of the CDRs

; 05/01/17: Left off at 14:00

















