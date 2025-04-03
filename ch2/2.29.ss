(define (make-mobile left right)
    (list left right))
(define (make-branch length structure)
    (list length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; weight
(define (total-weight mobile)
    (define (weight branch)
      (let ((struct (branch-structure branch)))
        (if (pair? struct)
            (total-weight struct) ; recurse over mobile
            struct)))
    (+ (weight (left-branch mobile)) (weight (right-branch mobile))))

; some mobiles
(define x (make-mobile (make-branch 3 4) (make-branch 4 7)))
(define y (make-mobile (make-branch 3 (make-mobile (make-branch 1 2) (make-branch 8 9))) (make-branch 4 7)))
(define z (make-mobile
              (make-branch 3 4)
              (make-branch 4 (make-mobile
                               (make-branch 6 (make-mobile
                                                (make-branch 3 5)
                                                (make-branch 6 7)))
                               (make-branch 12 3)))))

(total-weight x)
(total-weight y)
(total-weight z)

; balance test
(define (is-balanced mobile)
    ; approach: I want to percolate the weight up the tree.
    ; The following functions return a list containing three elements: (balanced-so-far, length, total-weight-so-far)
    (define (inspect-mobile mobile)
      ; to inspect a mobile, inspect its branches
      (let ((left (inspect-branch (left-branch mobile)))
            (right (inspect-branch (right-branch mobile))))
        ; the mobile is balanced if the branches are balanced and the arithmetic condition stands
        (list (and
                (car left)
                (car right)
                (= (* (cadr left) (caddr left)) (* (cadr right) (caddr right))))
          0 ; the mobile has no length of its own
          (+ (caddr left) (caddr right)) ; sum the weights of the branches
        )
      )
    )
    (define (inspect-branch branch)
      (let ((struct (branch-structure branch))
            (length (branch-length branch)))
        (if (pair? struct)
            ; for a mobile branch, inspect it and replace the length from the result with the branch length
            (let ((node (inspect-mobile struct)))
              (list (car node) length (caddr node)))
            ; for a simple branch return true and its length and weight
            (list #t length struct)
            )))
    (car (inspect-mobile mobile)))

; some balanced mobiles
(define a (make-mobile (make-branch 3 4) (make-branch 6 2)))
(define b (make-mobile (make-branch 5 (make-mobile (make-branch 2 4) (make-branch 4 2))) (make-branch 10 3)))
(define c (make-mobile
              (make-branch 6 8)
              (make-branch 4 (make-mobile
                               (make-branch 6 (make-mobile
                                                (make-branch 2 1)
                                                (make-branch 1 2)))
                               (make-branch 2 9)))))

; balanced
(is-balanced a)
(is-balanced b)
(is-balanced c)

; not balanced
(is-balanced x)
(is-balanced y)
(is-balanced z)

; alternative implementation:
; My original implementations of `total-weight` and `is-balanced` used `list?` test instead of `pair?`
; to check for sub-mobiles. After changing that, the only change necessary for the new constructors:  
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; is to redefine these two selectors
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

; note to self: remember to redefine the variables before testing those.
