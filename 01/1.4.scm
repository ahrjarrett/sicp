(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                x)))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (good-enough? guess x)
  ()

)

