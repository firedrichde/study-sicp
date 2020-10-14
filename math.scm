(define (even? x)
    (= 0 (remainder x 2)))

(define (average x y)
    (/ (+ x y) 2.0))

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


; 求解函数不动点
(define (fixed-point func first-guess)
    (define tolerance 0.0001)
    (define (close-enough? x y)
        (display y)
        (newline)
        (< (abs (- x y)) tolerance))
    (define (try-guess guess)
        (let ((next-guess (func guess)))
            (if (close-enough? guess next-guess)
                next-guess
                (try-guess next-guess))
            ))
    (display first-guess)
    (newline)
    (try-guess first-guess)            
)

; 连分式k项和
(define (cont-frac n d k)
    (define tolerance 0.0001)
    (define (close-enough? count x y)
        (> count k))
        ; (and (> count k) (< (abs (- x y)) tolerance)))
    (define (func n d count guess)
        (/ (n count) (+ (d count) guess)))
    (define (try-guess guess count)
        (let ((next-guess (func n d count guess)))
            (if (close-enough? count next-guess guess)
                    guess
                (try-guess next-guess (+ count 1)))
            ))
    (try-guess 1 1)            
)

; 
(define (cont-frac-count n d tolerance)
    (define (close-enough? count x y)
        ( < (abs (- x y)) tolerance))
    (define (func n d count guess)
        (/ (n count) (+ (d count) guess)))
    (define (try-guess guess count)
        (let ((next-guess (func n d count guess)))
            (if (close-enough? count next-guess guess)
                    count 
                (try-guess next-guess (+ count 1)))
            ))
    (try-guess 1 1)            
)

;导数 
(define (deriv f)
    (define dx 0.00001)
    (lambda (x)
            (/ (- (f (+ x dx))
                  (f x))
                dx
            )
    )    
)

(define (newton-transform f)
    (lambda (x)
            (- x
               (/ (f x)
                  ((deriv f) x)
               )
            )
    )
)

; 牛顿法求解函数零点
(define (newtons-method f guess)
    (fixed-point (newton-transform f) guess)
    )