; builds upon previous exercises. Tested using Racket's sicp-pict package.

(define (empty frame)
          ((segments->painter (list)) frame))

; modified corner-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter
                                (- n 1))))
        (let ((top-left (beside up empty))
              (bottom-right (below empty
                                   right))
              (corner (corner-split painter
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right
                         corner))))))

; tested with
; (paint (corner-split wave 3))

; modified square-limit
(define (square-limit painter n)
  (let ((combine4
         (square-of-four identity
                         flip-horiz
                         flip-vert
                         rotate180)))
    (combine4 (corner-split painter n))))


; tested with
; (paint (square-limit einstein 2))
