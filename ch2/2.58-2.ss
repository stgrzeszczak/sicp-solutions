; inspired by a solution from the net

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)       (eq? v1 v2)))

(define (sum? x)
  (pair? (memq '+ x)))

; unwraps single-element lists
(define (simplify expr)
    (if (= (length expr) 1)
        (car expr)
        expr))

; gathers everything before the first '+
(define (addend expr)
    (define (addend-impl expr)
      (if (eq? (car expr) '+)
        '()
        (cons (car expr) (addend-impl (cdr expr)))))
    (simplify (addend-impl expr)))

(define (augend expr)
    (simplify (cdr (memq '+ expr))))

; no need to modify it, as it is processed below (sum?) in the conditional in (deriv)
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (cddr p)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
            (make-product 
             (multiplier exp)
             (deriv (multiplicand exp) var))
            (make-product 
             (deriv (multiplier exp) var)
             (multiplicand exp))))
          (else (error "unknown expression 
                        type: DERIV" exp))))

; tested with
; > (deriv '(x + (3 * (x + (y + 2)))) 'x)
; 4
; > (deriv '(((x * x) + (x + 5)) + ((8 * x) * x)) 'x)
; (((x + x) + 1) + ((8 * x) + (8 * x)))

; and new cases for the 2nd part:
; > (deriv '(x + 3 * (x + y + 2)) 'x)
; 4
; > (deriv '(x + (x + y + 2) * 3) 'x)
; 4
; > (deriv '(x + (x + y + 2 * x) * 3) 'x)
; 10
; > (deriv '(x * (x + y + 2 * x) * 3) 'x)
; ((x * 9) + ((x + y + 2 * x) * 3))
; > (deriv '(3 * (x + y * 2) + x + 1) 'x)
; 4






