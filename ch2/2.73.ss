; 1.
; The lookup is done based on the operand ('+', '*', '**').
; For each operand, the code finds the relevant derivation procedure in the "table".
; For these two special cases of raw numbers and variables, there's no operand.

; 2.

; Parts 2. and 3. were tested using code for operation table from.
; https://mk12.github.io/sicp/exercise/2/4.html#ex2.73
; The actual implementation of the exercise is different than mk12's.

; use the operation of the expression as the tag 
(define (operator exp)
  (if (pair? exp)
      (car exp)
      (error "Bad operation expression:" exp)))

(define (operands exp)
  (if (pair? exp)
      (cdr exp)
      (error "Bad operation expression:" exp)))

; this is actually the same as the original make-sum from chapter 2.3, as attach-tag just prepends the list
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (attach-tag '+ (list a1 a2)))))

(define (sum-derivative operands var)
    (let ((addend (car operands)) (augend (cadr operands)))
      (make-sum (deriv addend var) (deriv augend var))))

(put 'deriv '+ sum-derivative)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (attach-tag '* (list m1 m2)))))

(define (product-derivative operands var)
    (let ((multiplier (car operands)) (multiplicand (cadr operands)))
      (make-sum
        (make-product multiplier (deriv multiplicand var))
        (make-product (deriv multiplier var) multiplicand))))

(put 'deriv '* product-derivative)

; 3.

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (attach-tag '** (list b e)))))

(define (exponent-derivative operands var)
    (let ((base (car operands)) (exponent (cadr operands)))
      (make-product exponent
        (make-product
          (make-exponentiation base (- exponent 1))
          (deriv base var)))))

(put 'deriv '** exponent-derivative)

; 4.

; use 'deriv as the type tag - this means that we need to store the operation in the expression

(define (operator exp)
  (if (and (pair? exp) (pair? (cdr exp)))
      (cadr exp)
      (error "Bad operation expression:" exp)))

(define (operands exp)
  (if (and (pair? exp) (pair? (cdr exp)))
      (cddr exp)
      (error "Bad operation expression:" exp)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (attach-tag 'deriv (list '* a1 a2)))))

(define (sum-derivative operands var)
    (let ((addend (car operands)) (augend (cadr operands)))
      (make-sum (deriv addend var) (deriv augend var))))

(put '+ 'deriv sum-derivative)

; and so on for other operations
