(define (iproduct term a next b)
    (define (iter a result)
      (if (> a b) result (iter (next a) (* result (term a))))
      )
    (iter a 1))

(define (rproduct term a next b)
    (if (> a b)
        1
        (* (term a) (rproduct term (next a) next b))
        ))

; I'm using the following function for each product term:
; h(n) = f(n)/g(n)  where n = 1,2 ....
; f(n) = n - (n % 2) + 2          ; 2, 4, 4, 6, 6
; g(n) = n + 2 - ( (n + 1) % 2 )  ; 3, 3, 5, 5, 7
(define (numerator n) (- (+ n 2) (remainder n 2)))
(define (denominator n) (- (+ n 2) (remainder (+ n 1) 2)))
(define (wallis n) (/ (numerator n) (denominator n)))

(define inc (lambda (x) (+ x 1)))

(display "(* 4.0 (iproduct wallis 1 inc 1000)): ")
(display (* 4.0 (iproduct wallis 1 inc 1000)))
(newline)
(display "(* 4.0 (iproduct wallis 1 inc 1001)): ")
(display (* 4.0 (iproduct wallis 1 inc 1001)))
(newline)
(display "(* 4.0 (rproduct wallis 1 inc 1000)): ")
(display (* 4.0 (rproduct wallis 1 inc 1000)))
(newline)
(display "(* 4.0 (rproduct wallis 1 inc 1001)): ")
(display (* 4.0 (rproduct wallis 1 inc 1001)))
(newline)

