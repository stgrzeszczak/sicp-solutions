(define (reverse sequence)
    (fold-right 
     (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
    (fold-left 
     (lambda (x y) (cons y x)) '() sequence))

; tested with
; (reverse (list 1 2 3 4))
; (4 3 2 1)

