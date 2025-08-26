(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op 
                          initial 
                          (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (queens board-size)
    (define (adjoin-position new-row k rest-of-queens) (append rest-of-queens (list new-row)))
    (define (safe? k positions)
      (define kth-row (list-ref positions (- k 1))) ; the new queen is at the end of the list
      (define (iter-is-safe col positions)
        (if (= col k)
            #t    ; we checked against all the queens
            (let ((row (car positions)))
              (if (or (= row kth-row)
                      (= (+ row col) (+ kth-row k))
                      (= (- row col) (- kth-row k)))
                  #f    ; queen is in check
                  (iter-is-safe (+ col 1) (cdr positions)))) ; else, check against the queen from the next column
        ))
      (iter-is-safe 1 positions))
    (define (queen-cols k)
      (if (= k 0)
          (list '()) ; start with a list containing an empty list - this is where the positions will be adjoined
          (filter
           (lambda (positions) 
             (safe? k positions))
           (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position 
                      new-row 
                      k 
                      rest-of-queens))
                   (enumerate-interval 
                    1 
                    board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size))


