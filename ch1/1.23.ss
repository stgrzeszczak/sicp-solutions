(define square (lambda (x) (* x x)))
(define (divides? a b)
    (= (remainder b a) 0))
(define (find-divisor n test-divisor)
    (define (next divisor)
          (if (> divisor 2) (+ divisor 2) 3)
    )
    (cond ((> (square test-divisor) n) 
           n)
          ((divides? test-divisor n) 
           test-divisor)
          (else (find-divisor 
                 n 
                 (next test-divisor)))))
(define (smallest-divisor n)
    (find-divisor n 2))
(define (prime? n)
    (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
    (display " *** ")
    (display (+ (time-nanosecond elapsed-time) (* 1000000000 (time-second elapsed-time)))))
(define (start-prime-test n start-time)
    (cond ((prime? n)
               (report-prime (time-difference (current-time) start-time))
               #t
               )
          (else #f)))
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (current-time)))

(display (timed-prime-test 1009))
(display (timed-prime-test 1013))
(display (timed-prime-test 1019))
(display (timed-prime-test 10007))
(display (timed-prime-test 10009))
(display (timed-prime-test 10037))
(display (timed-prime-test 100003))
(display (timed-prime-test 100019))
(display (timed-prime-test 100043))
(display (timed-prime-test 1000003))
(display (timed-prime-test 1000033))
(display (timed-prime-test 1000037))

; sample output: execution time drops by 55-65%. Hypothesis: the improvement is not 2x, because the test has
; we're adding an extra `if` to the code executed for every number - from `next` function.
; 1009 *** 4128#t   <--- bizarre outlier
; 1013 *** 632#t
; 1019 *** 416#t
; 10007 *** 825#t
; 10009 *** 967#t
; 10037 *** 755#t
; 100003 *** 1908#t
; 100019 *** 1916#t
; 100043 *** 1910#t
; 1000003 *** 6743#t
; 1000033 *** 6081#t
; 1000037 *** 7943#t


