(define (pascal x y)
        (if (or (= x y) (= y 1))
            1
            (+ (pascal (- x 1) (- y 1))
               (pascal (- x 1) y))))

(define (pascal2 x y)
    )