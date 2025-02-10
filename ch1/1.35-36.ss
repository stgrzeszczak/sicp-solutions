(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) 
         tolerance))
    (define (try guess)
      (newline)
      (display guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))

; fi^2 = fi + 1    /fi
; fi = 1 + 1/fi
(display "\nfi, starting point 1.0")
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
(display "\nfi, starting point 1")
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)
(display "\nsolution for x^x = 1000, starting from 2.0")
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

