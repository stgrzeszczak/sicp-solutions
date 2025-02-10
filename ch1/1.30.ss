(define (sum term a next b)
    (define (isum a result)
      (if (> a b)
          result
          (isum (next a) (+ result (term a)))))
    (isum a 0))

