(load "math.scm")

(define (smallest-divisor x)
    (find-divisor x 2))

(define (find-divisor x test-divisor)
    (if (divide? x test-divisor)
        test-divisor
        (find-divisor x (next-test-divisor test-divisor))))

(define (next-test-divisor x)
    (if (= 2 x)
        3
        (+ x 2)))

(define (divide? x y)
    (= 0 (remainder x y)))

(define (prime? x)
    (= (smallest-divisor x) x))

; 费马测试
(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* (expmod base (- exp 1) m) base) m))))

(define (fermat-test x)
    (define (try-it a)
        (= (expmod a x x) a)
    )
    (try-it (+ 1 (random (- x 1))))
)

(define (fast-prime? x times)
    (cond ((= 0 times) true)
        ((fermat-test x) (fast-prime? x (- times 1)))
        (else false)
    )
)