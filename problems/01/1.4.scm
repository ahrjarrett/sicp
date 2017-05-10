(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    ; take the avg of guess and x divided by guess
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1))

(define (average num1 num2)
  (/ (+ num1 num2) 2))

(display (sqrt 9))

;; Old attempt assigned variables to the global environment
;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x)
;                x)))
;
;(define (improve guess x)
;  (avg guess (/ x guess)))
;
;(define (good-enough? guess x)
;  ())
