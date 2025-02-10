(define (tan-cf x k)
      (define xx (* x x))
      (define (itan i accu)
        (if (= i 1)
            (/ x (- 1 accu))
            (itan (- i 1) (/ xx
                            (- (* 2 i) 1 accu)))
                 ))
      (itan k 0)
      )
(display "tangent of PI/4: ")
(display (tan-cf (/ 3.14159 4) 6))
(newline)
(display "tangent of PI/3: ")
(display (tan-cf (/ 3.14159 3) 6))
(newline)


