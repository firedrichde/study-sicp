; 丘奇计数
(define zero
    (lambda (f) (lambda (x) x))
)

(define (add-l n)
    (lambda (g)
            (lambda (x) (g ((n g) x)))
    ) 
)

(define one
    (lambda (f)
            (lambda (x) (f x))
    ) 
)

(define two 
    (lambda (f)
            (lambda (x) (f (f x))
    )))

(define (plus a b)
    (lambda (f)
            (lambda (x)
                    ((a f) ((b f) x))
            )
    ))

(define (inc x)
    (+ x 1)
)

