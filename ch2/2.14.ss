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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent center tolerance)
    (let ((half-width (* center tolerance 0.01)))
    (make-interval (- center half-width) (+ center half-width))))
(define (percent interval)
    (/ (* 100 (width interval)) (center interval)))

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

; testing
(define a (make-center-percent 100 5))
(define b (make-center-percent 100 10))

(display "(define r1 (par1 a b))")
(define r1 (par1 a b))
(newline)
(display "(define r2 (par2 a b))")
(define r2 (par2 a b))
(newline)
(display "(center r1): ")
(display (center r1))
(newline)
(display "(width r1): ")
(display (width r1))
(newline)
(display "(center r2): ")
(display (center r2))
(newline)
(display "(width r2): ")
(display (width r2))
(newline)
(display "(define r3 (div-interval a a))")
(define r3 (div-interval a a))
(newline)
(display "(center r3): ")
(display (center r3))
(newline)
(display "(width r3): ")
(display (width r3))
(newline)
(display "(define r4 (div-interval a b)))")
(define r4 (div-interval a b))
(newline)
(display "(center r4): ")
(display (center r4))
(newline)
(display "(width r4): ")
(display (width r4))
(newline)

(display "(define one (make-interval 1 1))")
(define one (make-interval 1 1))
(newline)
(display "(define r5 (div-interval one a)))")
(define r5 (div-interval one a))
(newline)
(display "(center r5): ")
(display (center r5))
(newline)
(display "(width r5): ")
(display (width r5))
(newline)

(display "(define r6 (add-interval a b)))")
(define r6 (add-interval a b))
(newline)
(display "(center r6): ")
(display (center r6))
(newline)
(display "(width r6): ")
(display (width r6))
(newline)

; analysis:
; 1/interval leads to small width for realistic intervals for resistors
; (with width much smaller than center).
; mul-interval and add-interval roughly add width.
; In the first method, we're diving the results of multiplication and addition,
; which both have accumulated width, leading to overall lower precision.
; in the second one, taking 1/R gives small width intervals. When they are summed,
; the accumulated width is still small, leading to higher precision of the final result.

