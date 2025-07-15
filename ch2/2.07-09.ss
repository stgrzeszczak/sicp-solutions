(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) 
                      (lower-bound y))
                   (+ (upper-bound x) 
                      (upper-bound y))))
(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) 
                 (lower-bound y)))
          (p2 (* (lower-bound x) 
                 (upper-bound y)))
          (p3 (* (upper-bound x) 
                 (lower-bound y)))
          (p4 (* (upper-bound x) 
                 (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))
(define (div-interval x y)
    (mul-interval x 
                  (make-interval 
                   (/ 1.0 (upper-bound y)) 
                   (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
    (make interval (- (lower-bound x)
                      (upper-bound y))
                   (- (upper-bound x)
                      (lower-bound y))))

; 2.09
; width for sum
; width(sum(x, y))
; (upper(sum(x, y)) - lower(sum(x, y))) / 2
; (upper(x) + upper(y) - lower(x) - lower(y)) / 2
; width(x) + width(y)

; width for substraction
; width(sub(x, y))
; (upper(x) - lower(y) - (lower(x) - upper(y))) / 2
; width(x) + width(y)

; example for multiplication
; (2,4) * (4,7) = (8, 28) ; widths 1, 1.5 and 10

; example for division
; (6,24) / (3,6) = (1, 8) ; widths 9, 1.5 and 3.5

