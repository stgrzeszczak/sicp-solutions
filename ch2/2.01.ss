(define (make-rat n d)
    (let ((g (gcd n d)))
      (let ((denom (/ d g)) (numer (/ n g)))
        (if (> denom 0)
            (cons numer denom)
            (cons (* -1 numer) (* -1 denom))
            ))))


