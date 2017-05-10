; LECTURE 3B:

; PART I: DERIVATIVES & REDUCTION

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



;; PART II: SYMBOLIC DIFFERENTIATION: QUOTATION

;; Regarding the messy output of running DERIV on
;; a procedure called foo--lots of unnecessary and
;; repeated computations:

;; "The procedure represents a set of local rules for
;; the expansion of the process, and here the process
;; left behind some stuff that was the answer, and it
;; was constructed by the walk it takes of the
;; tree structure, which is the expression."

;; Rewriting MAKE-SUM:

(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        (else (list '+ a1 a2))))

;; now DERIV of FOO with respect to X is much more succinct:
;; => (+ (* a (* x x)) b)

;; "I've chosen my representation to be the same as the representation
;; in my language of similar things. By doing so, I've invoked the
;; necessity, I've created the necessity, to have things like quotation.
;; Because of the fact that my language is capable of writing expressions
;; that talk about expressions of the language, I need ot have something
;; that says, 'This is the expression that I'm talking about', rather
;; than, 'This expression is talking about something, and I want to talk about that'"
