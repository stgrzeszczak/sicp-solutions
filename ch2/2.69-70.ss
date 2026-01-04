(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge nodes)
    (if (null? (cdr nodes)) ; if nodes has one element left
        (car nodes)         ; return the element
        (let ((left (car nodes)) (right (cadr nodes)) )
          (successive-merge (adjoin-set (make-code-tree left right) (cddr nodes)))
        )
    ))

; tested with
; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
; ((leaf A 4)
;   ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
;   (A B D C)
;   8)

; the result is the same as sample-tree from exercise 2.67

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (define (has-symbol symbol tree)
      (memq symbol (symbols tree)))
    (cond
      ((leaf? tree) '())
      ((has-symbol symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
      ((has-symbol symbol (right-branch tree)) (cons 1 (encode-symbol symbol (right-branch tree))))
      (else (error 'encode-symbol "symbol not found " symbol))
      )
    )

(define song-tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

; I can only encode the song in uppercase (it must match the symbols
(define encoded-song (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) song-tree))

; encoded-song
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1
;  1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1
;  0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

; (length encoded-song)
; 84

; if we used fixed-length code, each symbol would use 3 bits.
; The song has 36 symbols, so it would take 108 bits when encoded. 
