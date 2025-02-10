; single step fibonacci
; |1 1| * |a| = |a+b|
; |1 0|   |b|   |a  |

;Fib2(f(n),f(n-1)) = f(n+2),f(n+1)

; for n-th number, exponentiate the matrix to nth exponent 
; use fastexp-style O(log n) exponentiation

;Fib -> Fib2 -> Fib4 -> Fib8 -> Fib16

; |q+p q| * |q+p q| = |(q+p)^2+q^2  q*(q+p)+qp| = |2q^2 + 2qp + p^2  q^2+2qp| = |q'+p' q'|
; |q   p|   |q   p|   |q*(q+p)+qp   q^2+p^2   |   |q^2+2qp           q^2+p^2|   |q'    p'|
;
; ergo:
; p' = q^2+p^2
; q' = q^2 + 2qp

(define (fib n)
    (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
    (cond ((= count 0) 
           b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* q q) (* 2 p q))
                     (/ count 2)))
          (else 
           (fib-iter (+ (* b q) 
                        (* a q) 
                        (* a p))
                     (+ (* b p) 
                        (* a q))
                     p
                     q
                     (- count 1)))))
(display "(fib 0):")
(display (fib 0))
(newline)
(display "(fib 1):")
(display (fib 1))
(newline)
(display "(fib 2):")
(display (fib 2))
(newline)
(display "(fib 3):")
(display (fib 3))
(newline)
(display "(fib 4):")
(display (fib 4))
(newline)
(display "(fib 5):")
(display (fib 5))
(newline)
(display "(fib 6):")
(display (fib 6))
(newline)
(display "(fib 7):")
(display (fib 7))
(newline)
(display "(fib 1024):")
(display (fib 1024))
(newline)

