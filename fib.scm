(define (fib n)
    (define (fib-iter count a b)
            (if (> count n)
                b
                (fib-iter (+ count 1)
                          (+ a b)
                          a)))
    (fib-iter 1 1 0))
