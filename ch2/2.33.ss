(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op 
                        initial 
                        (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) 
                '() sequence))

; tested with
; (map (lambda (x) (+ x 1)) (list 1 2 4 7))
;(2 3 5 8)
; (define (square x) (* x x))
; (map square (list 1 2 3 4 5))
; (1 4 9 16 25)


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

; tested with
; (append (list 1 2 3 4 5) (list 2 4 7))
; (1 2 3 4 5 2 4 7)

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; tested with
; (length (list 1 23 45))
; 3
