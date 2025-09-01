(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter 
                                    (- n 1))))
          (below painter 
                  (beside smaller smaller)))))

(define (split combine-op combine-smaller-op)
  (define (split-impl painter n)
    (if (= n 0)
      painter
      (let ((smaller (split-impl painter (- n 1))))
        (combine-op painter (combine-smaller-op smaller smaller)))))
  split-impl)

