(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
     (define (irepeated fun i)
       (if (= i 1)
           fun
           (irepeated (compose fun f) (- i 1))
           ))
     (irepeated f n)
     )

(define (square n) (* n n))
(define (inc n) (+ 1 n))

(display "((repeated square 2) 5)")
(newline)
(display ((repeated square 2) 5))
(newline)
(display "((repeated inc 17) 5)")
(newline)
(display ((repeated inc 17) 5))

