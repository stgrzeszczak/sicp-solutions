(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

; verification
; (car (cons (x y)))
; (car (lambda (m) (m x y)))
; (lambda (m = (lambda (p q) p)) (m x y))
; ((lambda (p q) p) x y)
; x

(define (cdr z) 
  (z (lambda (p q) q)))
