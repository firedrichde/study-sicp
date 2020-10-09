; 使用new-if替代if会导致栈溢出,系统报错("Aborting!: maximum recursion depth exceeded")
(define (new-if predicate? then-clause else-clause)
    (cond (predicate? then-clause)
        (else else-clause)))

(define init-value 1.0)

(define difference 0.001)

(define (square x) (* x x))

(define (average x y)
        (/ (+ x y)
            2))

(define (sqrt x) (sqrt-iter init-value x))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
         guess
         (sqrt-iter (improve guess x)
                     x)))

(define (good-enough? guess x)
    (< (abs (- (square guess)
                x))
        difference))

(define (improve guess x)
        (average guess
                 (/ x guess)))