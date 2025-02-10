; crucial topic here: how many times to perform the Fermat test? The book gives no hints on this.
; I chose log(n), which in Chez Scheme means log_e(n)
; The test is run 7 to 14 times - for the numbers in the excercise

(define square (lambda (x) (* x x)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else #f)))

(define (report-prime elapsed-time)
    (display " *** ")
    (display (+ (time-nanosecond elapsed-time) (* 1000000000 (time-second elapsed-time)))))
(define (start-prime-test n start-time)
    (cond ((fast-prime? n (ceiling (log n)))
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

; sample output: execution is significantly slower(!) than in 1.23 for all numbers except those above 1M
; looks like the Fermat test itself is quite expensive?
; 1009 *** 13113#t
; 1013 *** 5595#t
; 1019 *** 10236#t
; 10007 *** 8379#t
; 10009 *** 3462#t
; 10037 *** 3600#t
; 100003 *** 5010#t
; 100019 *** 5046#t
; 100043 *** 5117#t
; 1000003 *** 6924#t
; 1000033 *** 6527#t
; 1000037 *** 6802#t


