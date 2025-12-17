; reusing code from previous exercises

(define (lookup key tree)
    (cond ((null? tree) #f)
          ((= key (entry tree)) (entry tree))
          ((< key (entry tree)) (lookup key (left-branch tree)))
          (else (lookup key (right-branch tree)))))


; tested with
; (define tree1 (make-tree 7
;                     (make-tree 3
;                       (make-tree 1 '() '())
;                       (make-tree 5 '() '()))
;                     (make-tree 9
;                       '()
;                       (make-tree 11 '() '()))))
; > (lookup 2 tree1)
; #f
; > (lookup 5 tree1)
; 5
; > (lookup 12 tree1)
; #f
; > (lookup 11 tree1)
; 11
; > (lookup 1 tree1)
; 1
