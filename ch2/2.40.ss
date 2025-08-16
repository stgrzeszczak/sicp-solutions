(define square (lambda (x) (* x x)))
(define (divides? a b)
    (= (remainder b a) 0))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) 
           n)
          ((divides? test-divisor n) 
           test-divisor)
          (else (find-divisor 
                 n 
                 (+ test-divisor 1)))))
(define (smallest-divisor n)
    (find-divisor n 2))
(define (prime? n)
    (= n (smallest-divisor n)))

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

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (unique-pairs n)
    (flatmap (lambda (i) ; for each i in 1..n
           (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1)))) ; for each j in 1..i-1 return (i j)
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum? (unique-pairs n))))

; tested with
; (prime-sum-pairs 6)
; ((1 2 3) (2 3 5) (1 4 5) (3 4 7) (2 5 7) (1 6 7) (5 6 11))

