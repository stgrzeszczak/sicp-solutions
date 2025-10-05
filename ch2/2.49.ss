; builds upon 2.46, 2.47, 2.48. Not copying them here. This code cannot be tested yet anyway without the draw-line function

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define (outline-painter frame)
      (let ((outline-segments (list
                        (make-segment (make-vect 0 0) (make-vect 0 1))
                        (make-segment (make-vect 0 0) (make-vect 1 0))
                        (make-segment (make-vect 0 1) (make-vect 1 1))
                        (make-segment (make-vect 1 0) (make-vect 1 1)))
                        ))
        ((segments->painter outline-segments) frame))
      )

(define (x-painter frame)
        (let ((x-segments (list
                        (make-segment (make-vect 0 1) (make-vect 1 0))
                        (make-segment (make-vect 0 0) (make-vect 1 1)))
                          ))
          ((segments->painter x-segments) frame))
        )

(define (diamond-painter frame)
        (let ((diamond-segments (list
                        (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                        (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                        (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                        (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))
                          ))
          ((segments->painter diamond-segments) frame))
        )

(define (wave frame)
        (let ((wave-segments (list
                              ; whatever ....
                              (make-segment (make-vect 0.2 0.0) (make-vect 0.35 0.6))
                              (make-segment (make-vect 0.8 0.0) (make-vect 0.65 0.6))
                              (make-segment (make-vect 0.35 0.6) (make-vect 0.2 0.5))
                              (make-segment (make-vect 0.65 0.6) (make-vect 0.8 0.5))
                              (make-segment (make-vect 0.35 0.0) (make-vect 0.5 0.3))
                              (make-segment (make-vect 0.65 0.0) (make-vect 0.5 0.3))
                              (make-segment (make-vect 0.2 0.5) (make-vect 0.05 0.7))
                              (make-segment (make-vect 0.8 0.5) (make-vect 0.95 0.3))
                              (make-segment (make-vect 0.35 0.6) (make-vect 0.35 0.8))
                              (make-segment (make-vect 0.65 0.6) (make-vect 0.65 0.8))
                              (make-segment (make-vect 0.45 0.6) (make-vect 0.45 0.9))
                              (make-segment (make-vect 0.55 0.6) (make-vect 0.55 0.9))
                              )
                          ))
          ((segments->painter wave-segments) frame))
        )
