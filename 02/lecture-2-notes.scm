; TODO: Need to define or import square

(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (1+ a) b))))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-sq (1+ a) b))))

(sum-int 2 5) ; 14
(sum-sq 5 8) ; 176
