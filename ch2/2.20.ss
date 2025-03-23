; Two interesting things here
; nil is not defined in Chez Scheme. I shall use (list) for empty list instead.
; (= a b) does not work on booleans. (equal? a b) does. Another option is (boolean=? a b). 
(define (same-parity x . rest)
    (define (filter-by-parity items expected)
      (cond ((null? items) (list))
            ((equal? expected (even? (car items)))
             (cons (car items) (filter-by-parity (cdr items) expected)))
            (else (filter-by-parity (cdr items) expected))
          )
      )
      (cons x (filter-by-parity rest (even? x))))

(same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
(same-parity 2 2 3 4 5 6 7)
; (2 2 4 6)
(same-parity 2 3 4 5 6 7)
; (2 4 6)

