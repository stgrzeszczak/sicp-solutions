(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; implementation 1
; store two points on diagonal

; accessors
; make get-x1 get-x2 get-y1 get-y2
(define (make p1 p2) (cons p1 p2))
(define (get-x1 rec) (x-point (car rec)))
(define (get-x2 rec) (x-point (cdr rec)))
(define (get-y2 rec) (y-point (cdr rec)))
(define (get-y1 rec) (y-point (car rec)))

; second layer
; length width
(define (length rec) (abs (- (get-x2 rec) (get-x1 rec))))
(define (width rec) (abs (- (get-y2 rec) (get-y1 rec))))

; third layer
; area perimeter
(define (area rec)
    (* (length rec) (width rec)))
(define (perimeter rec)
    (* 2 (+ (length rec) (width rec))))

; a test
(define a (make (make-point 6 7) (make-point -3 -1)))
(area a)
(perimeter a)

; implementation 2
; store point and a vector for the diagonal
; same accessor API with different implementation
; same second&third layer
; (although this is inefficient - we only need the vector to calculate the length/width/
; /area/perimeter, so perhaps a better implementation would be to redefine the second layer)
(define (make p v) (cons p v))
(define (get-x1 rec) (x-point (car rec)))
(define (get-y1 rec) (y-point (car rec)))
(define (get-x2 rec) (+ (x-point (car rec)) (x-point (cdr rec))))
(define (get-y2 rec) (+ (y-point (car rec)) (y-point (cdr rec))))

; a test
(define a (make (make-point 6 7) (make-point 4 5)))
(perimeter a)
(area a)
