(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

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

; testing
(define a (make-center-percent 100 20))
(center a)
(upper-bound a)
(width a)
(percent a)

