(define add-symbol '+)
(define multiply-symbol '*)
(define exponente-symbol '^)
(define reserved-words (list add-symbol multiply-symbol exponente-symbol))

(define (memq items x)
    (cond ((null? items) #f)
        ((eq? (car items) x) #t)
        (else (memq (cdr items) x)))
)

(define (variable? e)
    (and (symbol? e) (not (memq reserved-words e)))
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (sum? e)
    (and (pair? e) (eq? (car e) add-symbol))
)

(define (addend e)
    (car (cdr e))
)

(define (augend e)
    (if (not (sum? e))
        (error "is not a sum expression" e)
        (let ((remaining-expr (cdr (cdr e))))
            (display remaining-expr)
            (if (null? (cdr remaining-expr))
                (car remaining-expr)
                (append (list add-symbol) remaining-expr)))
        
    )
)

(define (make-sum a b)
    (display a)
    (display b)
    (cond 
        ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list add-symbol a b)))
)

(define (product? e)
    (and (pair? e) (eq? (car e) multiply-symbol))
)

(define (multiplier e)
    (car (cdr e))
)

(define (multiplicand e)
    (car (cdr (cdr e)))
)

(define (=number? expr num)
    (and (number? expr) (= num expr))
)

(define (make-product a b)
    (cond 
        ((and (number? a) (number? b)) (* a b))
        ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        (else (list multiply-symbol a b))
        )
)

(define (exponentiation? e)
    (and (pair? e) (eq? (car e) exponente-symbol))
)

(define (base e)
    (car (cdr e))
)

(define (exponentiation e)
    (car (cdr (cdr e)))
)

(define (make-exponentiation b exp)
    (cond ((=number? exp 0) 1)
        ((=number? exp 1) b)
        (else (list '^ b exp)))
)

(define (deriv expr var)
    (cond
        ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var)
            1
            0))
        ((sum? expr) (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
        ((product? expr) (make-sum (make-product (multiplicand expr) (deriv (multiplier expr) var))
                                  (make-product (multiplier expr) (deriv (multiplicand expr) var))
                        )
        )
        ((exponentiation? expr) 
            (let ((b (base expr))
                   (exp (exponentiation expr))
                 )
                (make-product (deriv b var)
                              (make-product exp
                                            (make-exponentiation b (make-sum exp -1))
                              )
                )
            ))
        (else (error "unknown expression type -- DERIV" expr))
    )
)

(define test-expr '(+ x 3))