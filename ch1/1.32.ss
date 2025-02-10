(define (raccumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (raccumulate combiner null-value term (next a) next b))
    ))

(define (product term a next b) (raccumulate * 1 term a next b))
(define (sum term a next b) (raccumulate + 0 term a next b))

(define (iaccumulate combiner null-value term a next b)
    (define (iter a accu)
      (if (> a b)
          accu
          (iter (next a) (combiner accu (term a)))))
    (iter a null-value))

(define (product term a next b) (iaccumulate * 1 term a next b))
(define (sum term a next b) (iaccumulate + 0 term a next b))

; both solutions tested using
(define incr (lambda (x) (+ x 1)))
(define id (lambda (x) x))
(define (square x) (* x x))

(product id 1 incr 5)
(sum square 2 incr 4)
