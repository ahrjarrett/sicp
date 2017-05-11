;; LECTURE 4A -- PATTERN-MATCHING and RULE-BASED SUBSTITUTION



;; PART I

;; "Why should we have to translate these calculus rules
;; into the language of the computer?"

;;     Pattern --- Rule ---> Skeleton
;;         |                    |
;;         |                    |
;;       Match            Instantiation
;;         |                    |
;;         |                    |
;;         V                    V
;;     Expression -------> Expression
;;      (source)            (target)

;; We're trying to find a solution to a class of problems,
;; rather than a particular problem. Therefore, we need to
;; bring the computer to our level of abstraction.

;; We need to separate the control structure of the type of problem,
;; for example instatiation or pattern matching, from the rules themselves
;; by encapsulating all the things that the two problems have in common.

;; For example, we're going to use `?` here to represent a "pattern variable",
;; but the point is that these rules aren't important, just that we recognize
;; the pattern of a left- and a right-side. Also, the use of the `:` represents
;; a "substitution object", or skeleton evaluations:

(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0 )
    ( (dd (?v v) (? v))              1 )
    ( (dd (?v u) (? v))              0 )

    ( (dd (* (? x1) (? x2)) (? v))
      (* (dd (: x1) (: v))
         (dd (: x2) (: v)))            )

    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2))) )

    ( (dd (** (? x) (?c n)) (? v))
      (* (* (: n)
            (** (: x) (: (- n :))))
         (dd (: x) (: v)))             )

  )
)


;; PATTERN MATCH:

;;         foo -- matches exactly foo
;;     (f a b) -- match a list whose 1st element is f, 2nd is a, 3rd is b
;;       (? x) -- matches anything, call it x
;;      (?c x) -- matches only constants
;;      (?v x) -- matches a variable which we call x


;; SKELETONS:
;;        foo -- instantiates itself
;;    (f a b) -- instantiates to a 3-list which are the result of
;;               instantiating each of f, a, b
;;      (: x) -- instantiates to the value of x in the pattern matched


;; So we're going to build a program that is a general-purpose simplifier,
;; so that we could do something like run this function:

(define dsimp
  (simplifier deriv-rules))

;; ...such that, given a set of rules, it will produce a procedure
;; which will simplify expressions containing the things that are
;; referred to by these rules.

;; For example if we write:
(dsimp '(dd (+ x y) x)) ;; => (+ 1 0)


;; We can also apply this to a set of algebraic expressions. We could write:
(define algebra-rules
  ('
   ( ((? op) (?c e1) (?c e2))
     (: (op e1 e2))                      )

   ( ((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1))              )

   ( (+ 0 (? e))                   (: e) )

   ( (* 1 (? e))                   (: e) )

   ( (* 0 (? e))                       0 )

   ;; We could even create arbitrarily complicated expressions, for example:
   ( (* (?c e1) (* (?c e2) (? e3)))
     (* (: (* e1 e2)) (: e3))            )

   ;; Or even things as complicated as the Distributive Law:
   ( (* (? c) (+ (? d) (? e)))
     (+ (* (: c) (: d)) (* (: c) (: e))) )
  )
)


;; These rules aren't the point; the point is the simplify program we're
;; going to write that will allow us to create these expressions arbitrarily.

;; We can think of these rules like a deck of cards, where each card
;; has a matching pattern and expression:

;;      |             RULE              |
;;      |-------------------------------|
;;      |      PATTERN -- EXPRESSION    |

;; The pattern is fed into a matcher and the expression is fed into
;; an instantiator, which creates a new expression/pattern that are again
;; passed into their respective processes, et cetera until the expression
;; is reduced into its simplest form.

;; (This seems to be the first reference to the EVAL -> APPLY circle.)



;; PART II: The Matcher

;; MATCHER FUNCTION takes a dictionary, a pattern and an expression that
;; is matched against the pattern.

;; Seen from a high-level perspective:
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         ... Atomic patterns)
        ... Pattern variable clauses
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
                (cdr exp)
                (match (car pat)
                       (car exp)
                       dict)))))

;; Atomic Pattern:
((atom? pat)
 (if (atom? exp)
     (if (eq? pat exp)
         dict
         'failed)
     'failed))

;; 3 kinds of Pattern Variables:
;;    1) arbitrary-constants
;;    2) arbitrary-variables
;;    3) arbitrary-expressions
((arbitrary-constant? pat)
 (if (constant? exp)
     (extend-dict pat exp dict)
     'failed))
((arbitrary-variable? pat)
 (if (variable? exp)
     (extend-dict pat exp dict)
     'failed))
((arbitrary-expression? pat)
 (extend-dict pat exp dict))

;; MATCH, now all put together:
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
                (cdr exp)
                (match (car pat)
                       (car exp)
                       dict)))))


;; INSTANTIATOR:
(define (instantiate skeleton dict)
  (define (loop s)
    (condo ((atom? s) s)
           ((skeleton-evaluation? s)
            (evaluate (eval-exp s) dict))
           (else (cons (loop (car s))
                       (loop (cdr s))))))
  (loop skeleton))


;; EVALUATE:
(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (mapcar (lambda (v)
                 (lookup v dict))
               (cdr form)))))



;; PART III: The Simplifier

;; High-level view:
(define (simplifier the-rules)
  (define (simplify-exp exp)
    ...)
  (define (simplify-parts exp)
    ...)
  (define (try-rules exp)
    ...)
  simplify-exp)

;; SIMPLIFY-EXP:
(define (simplify-exp exp)
  (try-rules (if (compound? exp)
                 (simplify-parts exp)
                 exp)))

;; SIMPLIFY-PARTS:
(define (simplify-parts exp)
  (if (null? exp)
      '()
      (cons (simplify-exp (car exp)))))

;; Or we could just use a higher-order procedure to
;; rewrite simplify-exp w/o CONS'ing the parts, using map:
(define (simplify-exp exp)
  (try-rules (if compound? exp)
             (map simplify-exp exp)
             exp))


;; TRY-RULES
;; Turns out TRY-RULES is a mess too, here's the high-level view:
(define (try-rules exp)
  (define (scan rules)
    ...)
  (scan the-rules))

;; SCAN
(define (scan rules)
  (if (null? rules)
      exp
      (let ((dict
             (match (pattern (car rules))
                    exp
                    (empty-dictionary))))
        (if (eq? dict 'failed)
            (scan (cdr rules))
            (simplify-exp
              (instantiate
                (skeleton (car rules))
                dict))))))


;; DICTIONARY
;; Again, this is "George's job" to implement, but so we can begin
;; to understand what a dictionary is, here's what's under the covers:
(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))


;; THOUGHTS (05/10/17):
;; Phew. Everything still feels like "magic".

