(define (cc amount coin-values)
    (define (no-more? coins) (null? coins))
    (define (except-first-denomination coins) (cdr coins))
    (define (first-denomination coins) (car coins))
    (cond ((= amount 0) 
           1)
          ((or (< amount 0) 
               (no-more? coin-values)) 
           0)
          (else
           (+ (cc 
               amount
               (except-first-denomination 
                coin-values))
              (cc 
               (- amount
                  (first-denomination 
                   coin-values))
               coin-values)))))
(define us-coins 
    (list 50 25 10 5 1))
(cc 100 us-coins)
; 292
(define uk-coins 
    (list 100 50 20 10 5 2 1 0.5))
(cc 100 uk-coins)
;104561
; Note: UK had a halfpenny coin until 1984. Without that coin, the result is 4563 

; The answer does not change because the algorithm explores all possible combination
; without assuming that subsequent coins will be smaller.
