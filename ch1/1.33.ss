(define (faccumulate filter combiner null-value term a next b)
      (define (iter a accu)
        (if (> a b)
            accu
            (if (filter a)
                (iter (next a) (combiner accu (term a)))
                (iter (next a) accu)
                )))
      (iter a null-value))

(define square (lambda (x) (* x x)))
(define (divides? a b)
    (= (remainder b a) 0))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) 
           n)
          ((divides? test-divisor n) 
           test-divisor)
          (else (find-divisor 
                 n 
                 (+ test-divisor 1)))))
(define (smallest-divisor n)
    (find-divisor n 2))
(define (prime? n)
    (= n (smallest-divisor n)))

(define incr (lambda (x) (+ x 1)))

(define (sum-primes-squared a b)
    (faccumulate prime? + 0 square a incr b))

(display "(sum-primes-squared 2 7): ") ; 4 + 9 + 25 + 49
(display (sum-primes-squared 2 7))
(newline)

(define id (lambda (x) x))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (product-of-rel-primes n)
    (define (is-rel-prime i) (= (gcd i n) 1))
    (faccumulate is-rel-prime * 1 id 1 incr (- n 1)))

(display "(product-of-rel-primes 4): ") ; 1 * 3
(display (product-of-rel-primes 4))
(newline)
(display "(product-of-rel-primes 6): ") ; 1 * 5
(display (product-of-rel-primes 6))
(newline)
(display "(product-of-rel-primes 7): ") ; 6!
(display (product-of-rel-primes 7))
(newline)
(display "(product-of-rel-primes 8): ") ; 3 * 5 * 7
(display (product-of-rel-primes 8))
(newline)

