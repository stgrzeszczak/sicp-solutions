(define (reverse lst)
    (define (iter in out)
      (if (null? in)
          out
          (iter (cdr in) (cons (car in) out))))
    (iter lst (list)))
(display "(reverse (list 1 4 9 16 25)): ")
(display (reverse (list 1 4 9 16 25)))

