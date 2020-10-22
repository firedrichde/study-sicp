(load "list.scm")
(load "map.scm")

(define (deep-reverse tree)
    (newline)
    (display tree)
    (reverse (map (lambda (x)
                 (cond ((not (pair? x)) x)
                     ((simple-list? x) (reverse x))
                     (else (deep-reverse x)))
                     )
                     tree)))

(define (acculate proc items)
    (if (null? items)
        nil
        (proc (car items) (acculate proc (cdr items))))
)

; 按从左往右的顺序列举树的叶子，输入为树，输出为表
(define (fringe tree)
    (acculate (lambda (x y)
                       (cond ((not (pair? x)) (append (list x) (fringe y)))
                           ((simple-list? x) (append x (fringe y)))
                           (else (append (fringe x) (fringe y))))
    ) tree)
)
; > (fringe (list 1 (list 2 3 4 (list 5)) (list 6 7) 8))
; >> (1 2 3 4 5 6 7 8)