;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1

; the base cases are kinda confusing w/ the returning zeroes,
; could probably refactor to make a simpler termination.
(define (pascal-triangle row col)
  (cond ((> col row) 0)
        ((< col 0) 0)
        ((= col 1) 1)
        ((+ (pascal-triangle (- row 1) (- col 1))
            (pascal-triangle (- row 1) col)))))

(pascal-triangle 5 2) ; => 4
