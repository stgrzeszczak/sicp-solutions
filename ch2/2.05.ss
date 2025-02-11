(define (fast-exp b n)
      (define (ifexp b n accu)
        (cond ((= n 0) accu)
          ((even? n) (ifexp (* b b) (/ n 2) accu))
          (else (ifexp b (- n 1) (* b accu)))
        ))
      (ifexp b n 1))

(define (cons a b) (* (fast-exp 2 a)
                        (fast-exp 3 b)))

; recursive definitions of car/cdr
(define (car pair)
      (if (= (remainder pair 2) 0)
          (+ 1 (car (/ pair 2)))
          0))
    )
(define (cdr pair)
      (if (= (remainder pair 3) 0)
          (+ 1 (cdr (/ pair 3)))
          0))
    )

; tested on
(car (cons 5 4))
(cdr (cons 5 4))

