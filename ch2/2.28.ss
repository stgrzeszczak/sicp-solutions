; in the following function, iterf recurses over the tree
; and prepends all elements into a single list in a depth-first-search approach.
; To get the final result, we need to reverse the result the iterf step. 
(define (fringe items)
    (define (reverse lst)
      (define (iter in out)
        (if (null? in)
            out
            (iter (cdr in) (cons (car in) out))))
      (iter lst (list)))
    (define (iterf in out)
      (cond ((null? in) out)
        ((not (pair? in)) (cons in out))
        (else
          (let ((next-out
                  (iterf (car in) out)
                  ))
            (iterf (cdr in) next-out)))
      ))
    (reverse
      (iterf items '())
      ))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list (list 1 2) (list 3 4 5)) (list (list 8 9 10) '() 11)))
(define z (list (list 1 2 3) (list 4 5)))

(display "x         : ")
(display x)
(newline)
(display "(fringe x): ")
(display (fringe x))
(newline)
(display "(fringe (list x x)): ")
(display (fringe (list x x)))
(newline)
(display "y         : ")
(display y)
(newline)
(display "(fringe y): ")
(display (fringe y))
(newline)
(display "z         : ")
(display z)
(newline)
(display "(fringe z): ")
(display (fringe z))
(newline)

