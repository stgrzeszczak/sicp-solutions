(define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op 
                          initial 
                          (cdr sequence)))))
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
    (accumulate-n cons '() mat))

; for every row in the matrix m,
;    for every column in matrix n,
;        calculate row . column
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
      (map (lambda (row)
             (map (lambda (col)
                    (dot-product row col))
                  cols))
           m)))

; tested with:
; (define vector '(2 1 2 1))
; (define matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)) )
; (define matrix2 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)) )

; (matrix-*-vector matrix vector)
; (14 31 44)

; (transpose matrix)
; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

; (matrix-*-matrix matrix matrix2)
; ((70 80 90) (126 147 168) (180 210 240))

