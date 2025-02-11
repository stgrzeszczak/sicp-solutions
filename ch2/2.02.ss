(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
    (define (avg x1 x2) (/ (+ x1 x2) 2))
    (let ((p1 (start-segment s)) (p2 (end-segment s)))
      (make-point (avg (x-point p1) (x-point p2)) (avg (y-point p1) (y-point p2)))))

(display "(define segment (make-segment (make-point -3 7) (make-point 2 -4))) ")
(define segment (make-segment (make-point -3 7) (make-point 2 -4)))
(newline)
(display "(print-point (midpoint-segment segment)) ")
(print-point (midpoint-segment segment))
(newline)

