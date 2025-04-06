(define (subsets s)
    (if (null? s)
        ; the list of subsets of the empty set contains one element - the empty set
        (list '())
        ; recursively generate the list of subsets for a list one element shorter
        (let ((rest (subsets (cdr s))))
          ; now, to generate the list of all subsets of s,
          ; we start with `rest` and duplicate each element of that list
          ; to either combine it with the first element of `s`, or not,
          ; thus generating all possible combinations.
          ; Mapping the prepender function over `rest` generates the first option for each element.
          ; Using intact `rest` provides the second option for each element.
          (append rest (map
                         (lambda (subset) (cons (car s) subset))
                         rest)))))

(subsets '(1 2 3))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

