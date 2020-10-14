(define (even? x)
    (= 0 (remainder x 2)))

(define (square x)
    (* x x))

(define (cube x)
    (* x x x))

(define (inc x)
    (+ x 1)
)

(define (identity x)
    x
)

(define (sum term low next high)
    (if (> low high)
        0
        (+ (term low) (sum term (next low) next high)))
)

; 使用迭代计算
(define (sum-iter term low next high)
    (define (iter product a)
        (if (> a high)
            product
            (iter (+ product (term a)) (next a)))
    )
    (iter 0.0 low)
)

(define (sum-integers a b)
    (sum identity a inc b)
)

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum-iter pi-term a pi-next b))

(define pi
    (* 8
       (pi-sum 1 100000))
)

(define (integral f a b dx)
    (define start (+ a (/ dx 2.0)))
    (define (integral-next x)
       (+ x dx))
    (* dx
       (sum-iter f start integral-next b))
)

; 使用辛普森法则求积分
(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (simpson-term k)
        (cond ((= 0 k) (f a))
              ((= n k) (f b))
              ((even? k) (* 2 (f (+ a (* k h)))))
              (else (* 4 (f (+ a (* k h)))))
              ))
    (/ (* h (sum simpson-term 0 inc n)) 3)
)


(define (product term low next high)
    (if (> low high)
        1
        (* (term low) (product term (next low) next high))))