(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op 
                          initial 
                          (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (sums-to triple s)
    (= s (+ (car triple) (cadr triple) (caddr triple))))

(define (unique-triples n)
    (flatmap (lambda (i)
             (flatmap (lambda (j) (map (lambda (k) (list k j i))
                                   (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))

(define (get-triples n s)
    (filter (lambda (triple) (sums-to triple s)) (unique-triples n)))

; tested with
; (get-triples 5 9)
; ((2 3 4) (1 3 5))
; (get-triples 5 8)
; ((1 3 4) (1 2 5))
; (get-triples 5 10)
; ((2 3 5) (1 4 5))
; (get-triples 7 10)
; ((2 3 5) (1 4 5) (1 3 6) (1 2 7))

