;; 06/05/17

;; This is an implementation of PUT, and GET as found on:
;; https://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on

;; Found it as I was working through
;; Lecture 4B on Generic Operators.


;; Initializing the data structure PUT returns
(define global-array '())

;; This one just makes a list of a key and a value:
(define (make-entry k v) (list k v))
;; This one accesses the key of an entry:
(define (key entry) (car entry))
;; This one accesses the value of the second item in the list.
;; (Ah! I see how this works. So simple, I would have complicated
;; it so much.)
(define (value entry) (cadr entry))


;; PUT here mutates global-array.
(define (put op type item)
  ;; PUT-HELPER: Checks if global-array is empty with NULL?
  ;; If so, it constructs a list that uses MAKE-ENTRY to
  ;; enter a key-value pair.
  (define (put-helper k array)
    ;; @@K: key you want to check
    ;; @@ARRAY: data structure you're checking for K
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))



