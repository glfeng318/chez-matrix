(import (matrix))

(define m '#(#(1 2 3)
             #(4 5 6)
             #(7 8 9)))

(define m2 '#(#(4 7)
              #(2 6)))

(pprint-matrix m)

(pprint-matrix (m+ m m))
(pprint-matrix (m- m m))
(pprint-matrix (m* m m))
(pprint-matrix (matrix-transpose m))

; -306
(printf "det=~a\n" (matrix-det '#(#(6 1 1) #(4 -2 5) #(2 8 7))))

; 96
(printf "det=~a\n" (matrix-det '#(#(1 2 3) #(8 0 4) #(7 6 5))))

; -1
(printf "det=~a\n" (matrix-det '#(#(1 2 0) #(0 1 3) #(2 7 8))))

; -134
(printf "det=~a\n" (matrix-det '#(#(1 7 2 4) #(1 5 2 4) #(3 0 1 0) #(2 1 5 -3))))

(pprint-matrix (matrix-det '#(#(3 4) #(6 8))))


(pprint-matrix (make-identity-matrix 5))


; '#(#(5 6) #(7 8)) => '#(#(-4 3) #(7/2 -5/2))
(pprint-matrix (matrix-inverse '#(#(5 6) #(7 8))))

; '#(#(4 7) #(2 6)) => '#(#(3/5 -7/10) #(-1/5 2/5))
(pprint-matrix (matrix-inverse '#(#(4 7) #(2 6))))

; '#(#(3 2 1) #(3 1 5) #(3 2 3)) => '#(#(7/6 2/3 -3/2) #(-1 -1 2) #(-1/2 0 1/2))
(pprint-matrix (matrix-inverse '#(#(3 2 1) #(3 1 5) #(3 2 3))))

; '#(#(3 4) #(6 8)) => error
(pprint-matrix (matrix-inverse '#(#(3 4) #(6 8))))
