; the following works, but it's not using the knowledge from 2.2 with insight. Look on the net for better solutions.

(define (deep-reverse items)
      (define (idr in out)
        (if (null? in) out
            (idr (cdr in) (cons (deep-reverse (car in)) out))
        ))
      (if (pair? items)
          (idr items '())
          items
      ))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list (list 1 2) (list 3 4 5)) (list (list 8 9 10) '() 11)))
(define z (list (list 1 2 3) (list 4 5)))

(display "x               : ")
(display x)
(newline)
(display "(deep-reverse x): ")
(display (deep-reverse x))
(newline)
(display "y               : ")
(display y)
(newline)
(display "(deep-reverse y): ")
(display (deep-reverse y))
(newline)
(display "z               : ")
(display z)
(newline)
(display "(deep-reverse z): ")
(display (deep-reverse z))
(newline)
