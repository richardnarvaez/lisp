; P.3.2. (Stark 90, pg. 91) Dar ejemplos de expresiones Lisp que se puedan evaluar sin error y
; tengan la siguiente estructura (x, y, z son expresiones):
; a) (x x)
; b) ((x y z))
; c) (x (y y) (z z))

(format t "~%3.2 A | El resultado es:")
(print ((LAMBDA (x) (+ x 1 x)) 7 ))

(format t "~%3.2 B | El resultado es:")
(print ((LAMBDA (x y z) (*(+ x y z) 2)) 1 2 3))

(format t "~%3.2 C | El resultado es:")
(print ((LAMBDA (x y z) (+ x (* y y) (/ z z))) 2 4 6 ))

El número de argumentos de la definición puede ser 0:
(print((LAMBDA NIL 7 8 9)))

; P.3.4. Supóngase que las funciones se escriben siempre en notación prefija y son asociativas por la
; derecha, es decir, fgh es la aplicación de f a la aplicación de g a h. Escribir en CommonLisp las
; siguientes definiciones lambda, y explicar qué hacen los objetos-procedimiento así definidos:
; a) λx.x
; b) λx.+xx
; c) λx.λy.x
; d) λx.λy.y
; e) λx.λy.λz.(xz)(yz)

(format t "~%3.4 A | El resultado es:")
(print ((LAMBDA (x y z) (+ x (* y y) (/ z z))) 2 4 6 ))

(
    DEFUN SUMATORIO (F I0 IN)
        (
            COND ((> I0 IN) 0)
                ( T (+ (FUNCALL F I0) (SUMATORIO F (1+ I0) IN)) )
        )
)

; P.3.6. Escribir definiciones Lisp de las siguientes funciones:

; a)la función delta, que tiene como argumento una función numérica f y devuelve el número f(0).

; b)la función búsqueda-áurea, que tiene como argumentos una función numérica f: R—>R y dos
; números a, b tales que f tiene exactamente un extremo, que es un mínimo, en el intervalo (a, b);
; búsqueda-áurea devuelve precisamente el valor de ese mínimo.

; c)la función integral, que tiene como argumentos una función numérica f:R—>R y dos números a,
; b; integral devuelve ∫ab f(x)dx. Supóngase que existe una constante deltax que establece el
; espaciamiento de los puntos del eje de las x en los que se calcula f(x).

; P.3.8. Definir las siguientes funciones:
; a) MI-FIND-IF, que tiene como argumentos un predicado p y una lista L, y devuelve el primer
; elemento de L que satisface p o falso si ninguno lo satisface.
(
    DEFUN MI-FIND-IF (P L)
        (
            COND ((NULL L) NIL)
            ((FUNCALL P (CAR L)) (CAR L))
            (T (MI-FIND-IF P (CDR L)))
        )
)

(
    DEFUN MI-FIND-IF (P L)
        (find P L)
)
(format t "~%~%MI-FIND-IF | El resultado es:")
(print(MI-FIND-IF 1 '(3 1 4 5 6 2)))

; b) MI-EVERY, que tiene como argumentos un predicado p y una lista L, y devuelve falso si algún
; elemento de L no satisface p o verdadero en otro caso.

(
    DEFUN MI-EVERY (P L)
        (COND ((NULL L) T)
            ((FUNCALL P (CAR L)) (MI-EVERY P (CDR L)))
            (T NIL))
)

(
    defun MI-EVERY (L n)     
    (cond ((null L) nil)      
        ((equal n (car L)) t)      
        ((listp (car L))       
        (or (deep-find (car L) n)        
        (deep-find (cdr L) n)))
        ((deep-find (cdr L) n))
    )
)
(format t "~%~%MI-FIND-IF | El resultado es:")
(print(MI-EVERY '("a" "b" "c") "a"))

; c) MI-COUNT-IF, que tiene como argumentos un predicado p y una lista L, y devuelve el número
; de elementos de L que satisfacen p.
(
    DEFUN MI-COUNT-IF (P L)
        (COND ((NULL L) NIL)
            ((FUNCALL P (CAR L)) T)
            (T (MI-COUNT-IF P (CDR L))))
)

(
    let ((hs (make-hash-table)))
    (
        defun mi-count-if (lst)
        (
            if lst  
            (multiple-value-bind (val exists)  
            (gethash (car lst) hs)  
            (if exists  
                (setf (gethash (car lst) hs) (1+ val))  
                (setf (gethash (car lst) hs) 1)
            )  
            (mi-count-if (cdr lst)))  hs
        )
    )
)
(print (gethash '1 (mi-count-if '(1 1 3 4 1 3 2 2 2 1 1))))

; d) Criticar la siguiente implementación de MI-FIND-IF:
; (DEFUN MI-FIND-IF (P L) (APPLY #'OR L))
    ; Aplica una función (que recibe como primer argumento) a una lista de argumentos. 
    ; Lo que hace es aplicar el OR a cada elemento de la lista lo que puede devolver el elemento deseado
    ; Ahora eso es lo que se pensaria en primera instancia no obstante nos devolveria un error
    ; que nos diria que el objeto-procedimiento no es una funcion.
    ; Un ejemplo de apply correcto puede ser  (apply #’+ ’(1 2 3)) => 6
    
; P.3.10. Definir las siguientes funciones:

; a)la función que a partir de un conjunto C devuelve su función característica χ(x), que vale 0 si
; x∉ C, 1 si x∈ C.
( defun elementode (lst s)
    ( cond 
        ((null lst) nil)
        ((eql (car lst) s) 1)
        (T 0)
    )
)

(print(elementode '(1 2 3 1 5 1 6 7 88 5) 1))

; b)la función proyección de una función de dos argumentos, definida como sigue: la función
; proyección tiene dos argumentos, una función f y un entero n = 1, 2, que indica si se trata de la
; primera o la segunda proyección. La n-ésima proyección de f se obtendrá haciendo NIL todos los
; argumentos de f salvo el n-ésimo.
(defmacro hacer-n-veces ((i max &optional res) &body body)
  `(do ((,i 0 (1+ ,i)))
       ((>= ,i ,max) ,res)
     ,@body))

; c)la función suavizar, que tiene como argumentos una función f : R—>R, un número natural n, un
; número real δ y una función suavización : R2n+1—>R. suavizar devuelve la función f suavizada
; según suavización, es decir, la función que a x le hace corresponder
; suavización(f(x - nδ), f(x - (n-1)δ), ... f(x + δ), f(x), f(x + δ), ...., f(x + nδ))

(defmacro iterar (steps (finp res) &body body)
  `(labels ((main ,(mapcar #'car steps)
                  (if ,finp ,res
                    (progn ,body
                           (main ,@(apply #'append
                                          (mapcar #'last steps)))))))
     (main ,@(mapcar #'cadr steps))))

(defmacro iterar.con.contador (steps (finp res) &body body)
  (let ((c (gensym "counter")))
    `(iterar ,(cons `(,c 0 (1+ ,c)) steps)
             (,finp (values ,res ,c))
             ,body)))

(defun suma.2 (x y)
  (iterar.con.contador ((total x (1+ total))
       (cont y (1- cont)))
      ((zerop cont) total)
    (print total) (print cont)))
