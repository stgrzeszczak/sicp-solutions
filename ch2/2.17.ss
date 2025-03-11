(define (last-pair lst)
    (if (null? (cdr lst))
        lst
        (last-pair (cdr lst))))
(display "(last-pair (list 23 72 149 34)): ")
(display (last-pair (list 23 72 149 34)))
