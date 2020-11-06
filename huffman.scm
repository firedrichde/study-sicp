(define leaf-symbol 'leaf)
(define (make-leaf char weight)
    (list leaf-symbol char weight)
)
(define (leaf? object)
    (and (pair? object) (eq? (car object) leaf-symbol))
)

(define (symbol-leaf object)
    (car (cdr object))
)

(define (weight-leaf object)
    (car (cdr (cdr object)))
)

(define (make-code-tree left right)
    (list left
          right
          (append (symbol-leaf left) (symbol-leaf right))
          (+ (weight-leaf left) (weight-leaf right))
    )
)

(define (left-branch tree)
    (car tree)
)

(define (right-branch tree)
    (car (cdr tree))
)

(define (symbol-tree tree)
    (if (leaf? tree)
        (symbol-leaf tree)
        (car (cdr (cdr tree)))
    )
)

(define (weight-tree tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
        )
)

(define (adjoin-set x set)
    (cond ((null? set) (list x))
        ((< (weight-tree x) (weight-tree (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))
        )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs))
            )
            )
        )
)

(define test-tree (list '(leaf a 1) '(leaf b 1) '(a b) 2))

(define test-pairs (list '(a 4) '(b 2) '(c 1) '(d 1)))