(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (add-vect v z) (make-vect (+ (xcor-vect v) (xcor-vect z)) (+ (ycor-vect v) (ycor-vect z))))
(define (sub-vect v z) (make-vect (- (xcor-vect v) (xcor-vect z)) (- (ycor-vect v) (ycor-vect z))))
(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; tested with
; (define a (make-vect 1 2))
; (define a (make-vect 3 5))
; (define a (make-vect 1 2))
; (define b (make-vect 3 5))
; (add-vect a b)
; (4 . 7)
; (sub-vect a b)
; (-2 . -3)
; (scale-vect 4 a)
; (4 . 8)

