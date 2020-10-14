(load "math.scm")

(define (factorial-a n)
    (if (= 1 n)
        1
        (* n (factorial-A (- n 1)))))


(define (factorial-b n)
    (define (factorial-B-iter product counter max-counter)
        (if (= counter max-counter)
            product
            (factorial-B-iter (* product counter)
                              (+ counter 1)
                              max-counter)))
    (factorial-B-iter 1 1 n))

(define (factorial-p n)
    (product identity 1 inc n)
)