; same implementation
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; always adjoin the element
(define (adjoin-set x set)
  (cons x set))

; same implementation
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

; always adjoin each member of set1 to set2
(define (union-set set1 set2)
    (cond ((null? set1) set2)
      (else (union-set (cdr set1) (cons (car set1) set2)))))

; the only operation that really improves efficiency-wise is adjoin-set.
; I cannot think of any application where that would suffice.

