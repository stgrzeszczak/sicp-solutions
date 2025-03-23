; this works, returns #t, even though the first case in the condition does not have a return value (!?)
; I still don't know how to break execution without returning something (as the built-in version for-each) 

(define (my-for-each f items)
    (cond ((null? items) )
      (else (f (car items)) (my-for-each f (cdr items)))
      )
    )

(my-for-each 
   (lambda (x) (newline) (display x))
   (list 57 321 88))

