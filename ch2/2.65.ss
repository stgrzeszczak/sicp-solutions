; my approach is lazy - to combine several O(n) procedures from previous exercises:
; tree->list-2 from 2.63 flattens the input trees
; intersection-set and union-set (here renamed to intersection-list-set and union-list-set) from 2.62 perform the operations
; list->tree from 2.64 builds the resulting tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (intersection-list-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-list-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-list-set 
                          set1 
                          (cdr set2)))))))

(define (intersection-set tree1 tree2)
    (list->tree (intersection-list-set
                  (tree->list-2 tree1)
                  (tree->list-2 tree2))))

(define (union-list-set set1 set2)
    (cond
      ((null? set1) set2)
      ((null? set2) set1)
      (else
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (union-list-set (cdr set1) (cdr set2))))
                ((< x1 x2) (cons x1 (union-list-set (cdr set1) set2)))
                ((< x2 x1) (cons x2 (union-list-set set1 (cdr set2)))))))))

(define (union-set tree1 tree2)
    (list->tree (union-list-set
                  (tree->list-2 tree1)
                  (tree->list-2 tree2))))

; tested using some imperfectly balanced trees
; (define tree1 (make-tree 7
;                    (make-tree 3
;                      (make-tree 1 '() '())
;                      (make-tree 5 '() '()))
;                    (make-tree 9
;                      '()
;                      (make-tree 11 '() '()))))
; (define tree2 (make-tree 2
;                    (make-tree 1 '() '())
;                    (make-tree 7
;                      (make-tree 3 '() '())
;                      (make-tree 9 '() (make-tree 12 '() '())))))
; > (intersection-set tree1 tree2)
; (3 (1 () ()) (7 () (9 () ())))
; > (union-set tree1 tree2)
; (5 (2 (1 () ()) (3 () ())) (9 (7 () ()) (11 () (12 () ()))))

