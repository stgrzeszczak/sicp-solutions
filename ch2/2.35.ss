(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op 
                        initial 
                        (cdr sequence)))))

(define (count-leaves t)
    (accumulate
      +
      0
      (map (lambda (tree)
             (cond
               ((not (pair? tree)) 1)
               (else (count-leaves tree))))
             t)))


; tested with
; (define x (list 1
;               (list 2 (list 3 4) 5) 
;               (list 6 7)))
; (count-leaves x)
; 7


