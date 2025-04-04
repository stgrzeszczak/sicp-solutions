(define dx 0.00001)
(define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
(define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) 
              ((deriv g) x)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) 
         tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))
(define (newtons-method g guess)
    (fixed-point (newton-transform g) 
                 guess))

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)
                          ))

(display "finding zero of (x-2)^3 with different starting points:")
(newline)
(display (newtons-method (cubic -6 12 -8) 3))
(newline)
(display (newtons-method (cubic -6 12 -8) 4))
(newline)

