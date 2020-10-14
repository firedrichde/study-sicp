(load "math.scm")
(load "fast-expt.scm")

(define (cubic-equation a b c)
    (define (cubic a b c)
        (lambda (x) (+ (cube x)
                       (* a (square x))
                       (* b x)
                       c
                    ))
    )
    (newtons-method (cubic a b c) 1)
)

(define (double-f  f)
    
    (lambda (x)
            (f (f x))
    )
)

; 复合函数 x-> f(g(x))
(define (compose  f g)
    (lambda (x) (f (g x)))
)

(define (repeated f n)
    (if (= 1 n)
        f
        (compose f (repeated f (- n 1)))
        )
)

(define (smooth f)
    (define dx 0.0001)
    (define (average x y z) (/ (+ x y z) 3.0))
    (lambda (x) (average (f x) (f (- x dx)) (f (+ x dx))))

)

(define (smooth-n f n)
    (repeated (smooth f) n))

(define (average-damp f)
    (lambda (x) (average (f x) x)))

(define (root-four x)
    (fixed-point (repeated (average-damp (lambda (y) (/ x (cube y)))) 2) 1.0)    
    )

(define (root-n-damp x n damp-times)
    (fixed-point (repeated (average-damp (lambda (y) (/ x 
                                                         (fast-expt y 
                                                                    (- n 1)))))
                            damp-times)
                  1.0)
)


(define (lg2 x)
    (cond ((> 1 (/ x 2)) 0)
        ((< 1 (/ x 2)) (+ 1 (lg2 (/ x 2))))
        (else 1)))

(define (root-n x n)
    (root-n-damp x n (lg2 n))
)

(define (iterative-improve good-enough improve-method)
    (lambda (guess)
        (define (try-guess x)
            (let ((next-guess (improve-method x)))
                (if (good-enough x next-guess)
                    next-guess
                    (try-guess next-guess))))
        (try-guess guess)
    )
)

; (define (sqrt-new x)
    ; )

(define (fixed-point-improve func first-guess)
    (define tolerance 0.0001)
    (define (good-enough guess next-guess)
        (< (abs (- guess next-guess)) tolerance))
    ((iterative-improve good-enough (average-damp func)) first-guess)
)

(define (sqrt-improve x)
    (fixed-point-improve (lambda (y) (/ x y)) 1.0)
)