(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

; 9 cases overall:
; for each x,y, the interval either has both bounds negative, one negative one positive,
; or both positive 
; 
; 4 cases where the intervals are wholly negative or positive
; if x > 0 and y < 0 return (upp(x)*low(y) low(x)*upp(y))
; this is because abs(upp(y)) < (abs(low(y)))
; if x < 0 and y > 0 return (low(x)*upp(y) upp(x)*low(y))
; this is because abs(upp(x)) < (abs(low(x)))
; if x > 0 and y > 0 return (low(x)*low(y) upp(x)*upp(y))
; if x < 0 and y < 0 return (upp(x)*upp(y) low(x)*low(y))

; 4 cases where one interval spans zero
; if x > 0, low(y) < 0 and upp(y) > 0 return (upp(x)*low(y) upp(x)*upp(y))
; if x < 0, low(y) < 0 and upp(y) > 0 return (low(x)*upp(y) low(x)*low(y))
; if low(x) < 0, upp(x) > 0 and y > 0 return (low(x)*upp(y) upp(x)*upp(y))
; if low(x) < 0, upp(x) > 0 and y < 0 return (upp(x)*low(y) low(x)*low(y))

; else if both intervals span zero, perform all 4 multiplications

(define (mul-interval x y)
      (define (spans interval)
        (and (< (lower-bound interval) 0) (>= (upper-bound interval) 0)))
      (define (is-positive interval)
        (>= (lower-bound interval) 0))
      (define (is-negative interval)
        (< (upper-bound interval) 0))
      (define (low interval) (lower-bound interval))
      (define (upp interval) (upper-bound interval))
      (define (long-mul x y)
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
      (let ((x-positive (is-positive x))
            (x-spans (spans x))
            (x-negative (is-negative x))
            (y-positive (is-positive y))
            (y-spans (spans y))
            (y-negative (is-negative y)))
        (cond (x-positive
                (cond (y-positive (make-interval (* (low x) (low y)) (* (upp x) (upp y))))
                      (y-spans    (make-interval (* (upp x) (low y)) (* (upp x) (upp y))))
                      (y-negative (make-interval (* (upp x) (low y)) (* (low x) (upp y))))))
              (x-negative
                (cond (y-positive (make-interval (* (low x) (upp y)) (* (upp x) (low y))))
                      (y-spans    (make-interval (* (low x) (upp y)) (* (low x) (low y))))
                      (y-negative (make-interval (* (upp x) (upp y)) (* (low x) (low y))))))
              (x-spans
                (cond (y-positive (make-interval (* (low x) (upp y)) (* (upp x) (upp y))))
                      (y-negative (make-interval (* (upp x) (low y)) (* (low x) (low y))))
                      (y-spans (long-mul x y))))
          )))

; a few tests:
(mul-interval (make-interval 6 8) (make-interval -4 -2))
(mul-interval (make-interval -6 8) (make-interval -2 4))
(mul-interval (make-interval -2 1) (make-interval -2 4))
(mul-interval (make-interval -8 -6) (make-interval -4 -2))
(mul-interval (make-interval -8 -6) (make-interval -2 4))

