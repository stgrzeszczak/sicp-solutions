(define x (list 1
              (list 2 (list 3 4) 5) 
              (list 6 7)))

; version with map
(define (square-tree tree)
    (define (sqt tree)
      (if (pair? tree)
          (square-tree tree)
          (* tree tree))
      )
    (map sqt tree))

; direct implementation
(define (square-tree tree)
    (cond
      ((null? tree) '())
      ((pair? tree)
       (cons (square-tree (car tree)) (square-tree (cdr tree))))
      (else (* tree tree)))  
    )

