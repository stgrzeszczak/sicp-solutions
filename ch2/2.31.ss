(define x (list 1
              (list 2 (list 3 4) 5) 
              (list 6 7)))

(define square (lambda (x) (* x x)))

; version with map
(define (tree-map f tree)
    (map (lambda (node)
           (if (pair? node)
               (tree-map f node)
               (f node)))
         tree))

; direct implementation
(define (tree-map f tree)
    (cond
      ((null? tree) nil)
      ((not (pair? tree)) (f tree))
      (else (cons (tree-map f (car tree)) (tree-map f (cdr tree))))
    ))

(define (square-tree tree) (tree-map square tree))

