(define (compose f g) (lambda (x) (f (g x))))
(define (square n) (* n n))
(define (inc n) (+ 1 n))
(display "result: ")
(display ((compose square inc) 6))

