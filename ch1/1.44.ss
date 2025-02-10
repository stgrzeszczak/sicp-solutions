(define dx 0.00001)
(define (smooth f)
       (lambda (x) (/ (+
                      (f (- x dx))
                      (f x)
                      (f (+ x dx))
                      )
                      3)
         ))

(define (square n) (* n n))

(display "((smooth square) 4): ")
(newline)
(display ((smooth square) 4))
(newline)

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
     (define (irepeated fun i)
       (if (= i 1)
           fun
           (irepeated (compose fun f) (- i 1))
           ))
     (irepeated f n)
     )

(display "(((repeated smooth 2) square) 4): ")
(newline)
(display (((repeated smooth 2) square) 4))
(newline)
