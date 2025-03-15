(define (last-pair lst)
    (let ((next (cdr lst)))
      (if (null? next)
          lst
          (last-pair next))))

(display "(last-pair (list 23 72 149 34)): ")
(display (last-pair (list 23 72 149 34)))
