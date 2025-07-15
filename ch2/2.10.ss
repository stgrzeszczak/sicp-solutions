(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

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

(define (width-interval i)
    (/ (- (upper-bound i)
          (lower-bound i))
       2))

(define (div-interval x y)
    (define (spans-zero interval)
      (and (< (lower-bound interval) 0) (>= (upper-bound interval) 0)))
    (if (spans-zero y)
        (error "div-interval" "cannot divide by interval with zero span")
        (mul-interval x
          (make-interval
            (/ 1.0 (upper-bound y))
            (/ 1.0 (lower-bound y))))))


; tested using
(div-interval (make-interval 6 24) (make-interval -7 7))
