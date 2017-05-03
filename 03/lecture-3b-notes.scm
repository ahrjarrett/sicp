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

; WATCH VIDEO AGAIN: 10:00 to 13:00 to get this correct...
; also, is this function also called deriv?

(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (deriv (m1 exp) var)
                        (m2 exp))))
        ; etc...
        ; ...
        ; ...
        ; ...
        ))


; Procedures are list structure, where the operator is the CAR
; and the operands are the successive CARs of the CDRs.
; So let's start breaking deriv into parts:

(define constant? exp var
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

; NEW SYNTAX: quotation mark before an operator
; "Quotation is a very complex topic."
(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

; NEW PRIMITIVE:
; CADR is the CAR of the CDR of something.
(define (make-sum a1 a2)
  (list '+ a1 a2))

; CADR -> CAR of the CDR (the CAR being the + operator in the list)
; CADDR -> CAR of the CDR of the CDR... and so on
(define a1 cadr)
(define a1 caddr)

(define (product? exp)
  (and (not (atom? exp)
            (eq? (car exp) '*))))

(define (make-product m1 m2)
  (list '* m1 m2))

(define m1 cadr)
(define m2 caddr)
