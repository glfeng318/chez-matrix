;; matrix.ss
;; tiny matrixlibrary
;;

(library
  (matrix)
  (export make-matrix
          make-identity-matrix
          matrix? square-matrix?
          matrix-ref matrix-set!
          m+ m- m* matrix-transpose matrix-det matrix-inverse
          pprint-matrix)
  (import (chezscheme))

  ;; make-matrix
  (define make-matrix
    (lambda (row col)
      (do ((m (make-vector row))
          (i 0 (+ i 1)))
          ((= i row) m)
          (vector-set! m i (make-vector col)))))
  
  ;; make-identity-matrix
  (define make-identity-matrix
    (lambda (n)
      (let ((m (make-matrix n n)))
        (do ((i 0 (+ i 1)))
            ((= i n) m)
            (matrix-set! m i i 1)))))

  ;; matrix?
  (define matrix?
    (lambda (m)
      (and (vector? m)
           (> (vector-length m) 0)
           (vector? (vector-ref m 0)))))
  ;; square-matrix?
  (define square-matrix?
    (lambda (m)
      (= (matrix-rows m) (matrix-cols m))))

  ;; matrix-ref
  (define matrix-ref
    (lambda (m row col)
      (vector-ref (vector-ref m row) col)))

  ;; matrix-set!
  (define matrix-set!
    (lambda (m row col x)
      (vector-set! (vector-ref m row) col x)))
  
  ;; matrix-rows
  (define matrix-rows
    (lambda (x)
        (vector-length x)))

  ;; matrix-cols
  (define matrix-cols
    (lambda (x)
        (vector-length (vector-ref x 0))))

  ;; m+
  (define (m+ m1 m2) (m± m1 m2 +))
  (define (m- m1 m2) (m± m1 m2 -))
  (define (m± m1 m2 op)
    (if (and (matrix? m1) (matrix? m2))
      (let* ((row1 (matrix-rows m1))
             (row2 (matrix-rows m2))
             (col1 (matrix-cols m1))
             (col2 (matrix-cols m2))
             (r   (make-matrix row1 col2)))
            (if (not (and (= row1 row2) (= col1 col2)))
              (error (if (eq? op +) 'm+ 'm-) "shapes of ~s and ~s are not equaled" m1 m2))
            (do ((i 0 (+ i 1)))
                ((= i row1) r)  ; row
                (do ((j 0 (+ j 1)))
                    ((= j col2))  ; col
                    (matrix-set! r i j (op (matrix-ref m1 i j) (matrix-ref m2 i j))))))
      (error (if (eq? op +) 'm+ 'm-) "~s or ~s is not a matrix" m1 m2)
     ))
  
  ; m*
  (define (m* m1 m2)
    (let (; number * matrix
          (x*m (lambda (x m)
                 (let* ((nr (matrix-rows m))
                        (nc (matrix-cols m))
                        (r  (make-matrix nr nc)))
                   (do ((i 0 (+ i 1)))
                       ((= i nr) r)
                       (do ((j 0 (+ j 1)))
                           ((= j nc))
                           (matrix-set! r i j (* x (matrix-ref m i j))))))))
          ; matrix * matrix
          (m*m (lambda (m-1 m-2)
                 (let* ((row1 (matrix-rows m-1))
                        (row2 (matrix-rows m-2))
                        (col2 (matrix-cols m-2))
                        (new-matrix  (make-matrix row1 col2)))
                   (if (not (= (matrix-cols m-1) row2))
                       (error 'm* "incompatible operand types: ~s and ~s" m-1 m-2))
                     (do ((i 0 (+ i 1)))
                         ((= i row1) new-matrix)
                         (do ((j 0 (+ j 1)))
                             ((= j col2))
                             (do ((k 0 (+ k 1))
                                 (a 0 (+ a (* (matrix-ref m-1 i k) (matrix-ref m-2 k j)))))
                                 ((= k row2)
                                  (matrix-set! new-matrix i j a)))))))))
      ;
      (cond
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (number? m1) (matrix? m2)) (x*m m1 m2))
        ((and (matrix? m1) (number? m2)) (x*m m2 m1))
        ((and (matrix? m1) (matrix? m2)) (m*m m1 m2))
        (else (error 'm* "incompatible operand types: ~s and ~s" m1 m2)))
      ))

  ; matrix transpose
  (define (matrix-transpose m)
    (let* ((nr (matrix-rows m))
           (nc (matrix-cols m))
           (new-matrix (make-matrix nc nr)))
      (do ((i 0 (+ i 1)))
          ((= i nr) new-matrix)
          (do ((j 0 (+ j 1)))
              ((= j nc))
              (matrix-set! new-matrix j i (matrix-ref m i j))))
      ))
  ; determinant
  (define (matrix-det m)
    (let* ((n (matrix-rows m))
           (n1 (- n 1))
           (sub-matrix (make-matrix n1 n1)))
      (when (not (= n (matrix-cols m)))
        (error 'matrix-det "~s is not a square matrix" m))
      (if (= n 2)
        (- (* (matrix-ref m 0 0) (matrix-ref m 1 1)) (* (matrix-ref m 0 1) (matrix-ref m 1 0)))
        ; iterate column
        (do ((c 0 (+ c 1))
             (det 0 (+ det (* (if (= (mod c 2) 1) -1 1) (matrix-ref m 0 c) (matrix-det sub-matrix)))))
            ((= c n) det)
            (begin
              (set! sub-matrix (make-matrix n1 n1))
              ; make sub matrix: r1 start from 1 (ignore the 1st row)
              (do ((r1 1 (+ r1 1)))
                  ((= r1 n))
                  (do ((c1 0 (+ c1 1)))
                      ((= c1 n))
                      (when (not (= c1 c))
                        (matrix-set! sub-matrix (- r1 1) (if (> c1 c) (- c1 1) c1) (matrix-ref m r1 c1))))
                    )))
        )))
  
  ; inverse
  (define matrix-inverse
    (lambda (m)
      (when (= (matrix-det m) 0)
        (error 'matrix-inverse "det of ~s is zero" m))
      (let* ((n (matrix-rows m))
             (identity-matrix (make-identity-matrix n))
             (inverse-matrix (make-matrix n n))
             (m2 (make-matrix n (* 2 n)))
             (t 0.0))
        ; combine m and identity-matrix
        (do ((i 0 (+ i 1)))
            ((= i n))
            (do ((j 0 (+ j 1)))
                ((= j n))
                (begin
                  (matrix-set! m2 i j (matrix-ref m i j))
                  (matrix-set! m2 i (+ j n) (matrix-ref identity-matrix i j)))))
        
        ; Gauss-Jordan Elimination
        (do ((i 0 (+ i 1)))
            ((= i n))
            (begin
              (set! t (matrix-ref m2 i i))
              (do ((j i (+ j 1)))
                ((= j (* 2 n)))
                (matrix-set! m2 i j (/ (matrix-ref m2 i j) t))
                )
              (do ((j 0 (+ j 1)))
                  ((= j n))
                  (when (not (= i j))
                    (set! t (matrix-ref m2 j i))
                    (do ((k i (+ k 1)))
                        ((= k (* 2 n)))
                        (matrix-set! m2 j k (- (matrix-ref m2 j k) (* t (matrix-ref m2 i k))))
                        )
                    )
                  )))
        
        ; fill inverse-matrix
        (do ((i 0 (+ i 1)))
            ((= i n) inverse-matrix)
            (do ((j 0 (+ j 1)))
                ((= j n))
                (matrix-set! inverse-matrix i j (matrix-ref m2 i (+ j n)))
                ))


        )
      ))

  ; prety print matrix  
  (define (pprint-matrix m)
    (let ([row (matrix-rows m)])
      (do ((r 0 (+ r 1)))
          ((= r row))
          (printf "~a~a~a\n" (if (= r 0) "#(" "  ") (vector-ref m r) (if (= r (- row 1)) ")" "")))))
  
  
  )
