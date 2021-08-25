( defun elementode (lst s)
    ( cond 
        ((null lst) nil)
        ((eql (car lst) s) 1)
        (T 0)
    )
)

(print(elementode '(1 2 3 1 5 1 6 7 88 5) 1))