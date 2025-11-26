(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

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

; 2.63 part 1
; I don't see how these procedures' results would differ, so I'll just check them out using the provided trees:

(define tree1 (make-tree 7
                  (make-tree 3
                    (make-tree 1 '() '())
                    (make-tree 5 '() '()))
                  (make-tree 9
                    '()
                    (make-tree 11 '() '()))))

(define tree2 (make-tree 3
                  (make-tree 1 '() '())
                  (make-tree 7
                    (make-tree 5 '() '())
                    (make-tree 9 '() (make-tree 11 '() '())))))

(define tree3 (make-tree 5
                  (make-tree 3 (make-tree 1 '() '()) '())
                  (make-tree 9
                    (make-tree 7 '() '())
                    (make-tree 11 '() '()))))

; (tree->list-1 tree1)
; (1 3 5 7 9 11)
; > (tree->list-2 tree1)
; (1 3 5 7 9 11)
; > (tree->list-1 tree2)
; (1 3 5 7 9 11)
; > (tree->list-2 tree2)
; (1 3 5 7 9 11)
; > (tree->list-1 tree3)
; (1 3 5 7 9 11)
; > (tree->list-2 tree3)
; (1 3 5 7 9 11)
; tentative conclusion: there is no difference

; 2.63 part 2
; For a balanced tree, tree-list-2 performs as O(n), because each entry is processed exactly once,
; whereas tree-list-1 performs as O(n*log(n)), because of the use of (append) for level of the tree.
; Let's use a simplified view of the tree:
;        n
;      /   \
;     /     \
;   1/2n     1/2n
;   /\        / \
;  /  \      /   \
; 1/4n 1/4n 1/4n  1/4n
;
; There are log(n) levels. For each level, we need to append 1/2 * n elements from the lists collecting
; the left branches to the lists collecting the right branches. So the number of steps just from appending
; is O(n * log(n)).
; (In the worst unbalanced case, tree-list-1 has O(n^2) order of growth.)
