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

; book code modified to use Chez scheme time primitives and to return whether test result was true/false
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


(define (search-for-primes n)
    (define (sfpi n count)
      (if (timed-prime-test n)
          (if (= count 2)
             (newline)   ; if previous count was 2, new count is 3, so stop recursion and print newline instead
             		 ; - but how do I just break the execution without making some call ????
             		 ; Also: I'm getting some "void"s in the printed output
             (sfpi (+ n 1) (+ count 1)))
          (sfpi (+ n 1) count)
          )
      )
    (sfpi n 0)
    )
    
; results (abridged)

; (search-for-primes 1000)
; 1009 *** 844
; 1013 *** 789
; 1019 *** 774

; (search-for-primes 10000)
; 10007 *** 1710
; 10009 *** 1455
; 10037 *** 1432

; (search-for-primes 100000)
; 100003 *** 5939
; 100019 *** 5837
; 100043 *** 5823

; (search-for-primes 1000000)
; 1000003 *** 14355
; 1000033 *** 13688
; 1000037 *** 16241

; the time jump from tests for 1000 to tests for 10000 is not scaling by sqrt(10) - it's about 2x, not 3x.
; The further jumps are closer to this result.

; when loading the script wholesale, the results are closer to sqrt(10) - effect of branch prediction?

(display "(search-for-primes 1000):")
(display (search-for-primes 1000))
(newline)
(display "(search-for-primes 10000):")
(display (search-for-primes 10000))
(newline)
(display "(search-for-primes 100000):")
(display (search-for-primes 100000))
(newline)
(display "(search-for-primes 1000000):")
(display (search-for-primes 1000000))
(newline)
