; LECTURE 3B: [title]



; PART I: HIGHER-ORDER PROCEDURES

; Returning to CONS, CAR & CDR

; we’re making a vector, effectively graphing it
; here we describe make-vector in terms of CONS, CAR & CDR:
(define make-vector cons)
(define xcor car)
(define ycor cdr)
; note: we’re not defining them in terms of their params

; if we describe 2 vectors and draw a line between their endpoints,
; we can represent line segments in terms of the relationship btwn them
(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

; Returning to the notion of CLOSURE:
; “Remember that closure is the thing that allowed us to start
; building up complexity, that didn't trap us in pairs.”

; A mathematician might say: The set of data objects in LISP
; is closed under the operation of forming pairs.”


; In LISP, chaining pairs is called a LIST.
; Just a convention for representing the sequence.
; CAR points to a value and CDR points to the next CAR
(cons 1
  (cons 2
    (cons 3
      (cons 4 nil) ; nil a special marker signifying the end of the list

; syntactic sugar for defining a list:
(list 1 2 3 4)
    ; if we define 1-to-4:
(define 1-to-4 (list 1 2 3 4)
    ; let’s say we wanted to access 2:
(car (cdr 1-to-4)) ; => 2
    ; let’s say we wanted to access 3:
(car (cdr (cdr 1-to-4))) ; => 3
    ; what if we want an empty list?
(car (cdr (cdr 1-to-4))) ; => ()

; we can write procedures that operate on every member of a list (a functor right?)
; what if we wrote something list this:
(scale-list 10 1-to-4) ; => (10, 20, 30, 40)

; how might we write that?
(define (scale-list s l)
  (if (null? l)
      nil
      (cons (* (car l) s)
            (scale-list s (cdr l)))))

; MAP:
; but of course, we shouldn’t even write scale-list at all,
; we should be writing the higher-order procedure
; and define scale-list in terms of that
(define (map fn list)
  (if (null? list)
      nil
      (cons (fn (car list))
            (map fn (cdr list)))))

; defining scale-list in terms of MAP:
(define (scale-list scale list)
  (map (lambda (item) (* item scale))
       list))

; we could have easily written
; MAP as an iterative process instead of a recursive one,
; but it wouldn’t matter to us when we write scale-list.

; APL:
; The idea that we don’t have to think about the particular
; control flow and structure of these higher order fns is
; something that came from APL.

; From the preface of ‘A Programming Language’, 1962:
; “Applied mathematics is largely concerned with the design
; and analysis of explicit procedures for calculating the exact
; or approximate values of various functions. Such explicit
; procedures are called algorithms or programs. Because an
; effective notation for the description of programs exhibits
; considerable syntactic structure, it is called a programming language.”

; FOR-EACH
; very very similar, but it seems like it slightly implies side-effects?
(define (for-each proc list)
  (cond ((null? list) “done”)
        (else (proc (car list))
              (for-each (cdr list)))))



; PART II: [title]






















