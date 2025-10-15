(define (are-equal l1 l2)
    (cond
      ((and (pair? l1) (pair? l2))
       (and (are-equal (car l1) (car l2)) (are-equal (cdr l1) (cdr l2))))
      ((and (null? l1) (null? l2)) #t)
      ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
      (else #f)
      )
    )

; tested with
; (are-equal '(this is a list) '(this is a list))
; #t
; (are-equal '(this is a list) '(this (is a) list))
; #f
; (are-equal '((this is) a list) '((this is) a list))
; #t
; (are-equal '(this is a list (5)) '(this is a list (5)))
; #t
; (are-equal '(this is a list (5)) '(this is a list (6)))
; #f
