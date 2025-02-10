(define (double f) (lambda (x) (f (f x)) ))
(define (inc n) (+ 1 n))
(display "double once: ")
(display (( double inc) 5))
(newline)
(display "double twice: ")
(display (((double double) inc) 5))
(newline)
(display "double 3x: ")
(display (((double (double double)) inc) 5))
;zonk!
(newline)

