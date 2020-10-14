(define (fast-expt b n)
    (define (fast-expt-iter a m)
        (cond ((= m 0) a)
            ; ((and (even? n ) (= m n)) (fast-expt-iter b (- m 1)))
            ((even? m) (fast-expt-iter (square a) (/ m 2)))
            (else (fast-expt-iter (* a b) (- m 1)))))
    (if (even? n)
        (* b (fast-expt-iter 1 (- n 1)))
        (fast-expt-iter 1 n)))

(define (square x) (* x x))

(define (even? n)
    (= (remainder n 2) 0))

(define (double x)
    (+ x x))

(define (halve x) 
    (/ x 2))


(define (fast-multiply a b)
    (define (fast-multiply-iter x y)
        (cond ((= y 1) x)
              ((even? y) (fast-multiply-iter (double x) (halve y)))
          (else (+ x (fast-multiply-iter x (- y 1))))))
    (fast-multiply-iter a b))

(define (fast-multiply2 a b)
    (define (iter x y product)
        (cond ((= y 0) product)
            ((even? y) (iter (double x) (halve y) product))
            (else (iter x (- y 1) (+ product x)))))
    (iter a b 0))