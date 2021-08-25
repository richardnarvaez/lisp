
; 1. **Sean dos listas L1, L2. El apareamiento de L1 y L2 es la lista formada por listas de dos **elementos, tomados el primero de L1 y el segundo de L2. Por ejemplo, el apareamiento de (DO RE **MI) y (C D E) es ((DO C) (RE D) (MI E)). Si las listas son de distinta longitud, se aparean hasta que **una de ellas se acabe. Por ejemplo, (A) y (X Y Z) dan ((A X)). Escribir en LISP la definición **recursiva de la función de apareamiento.**
(
    defun apareamiento (L M R)
        (SETF X (POP L))
        (SETF Y (POP M))
        (SETF Z NIL)
        (PUSH X Z)
        (PUSH Y Z)
        (PUSH Z R)
        (
            IF (= (LENGTH L)0)  
                (PRINT R)  
                (APAREAMIENTO L M R)
        )
) 
(print(APAREAMIENTO '(A1 A2 A3) '(B1 B2 B3) NIL))
; 1. **Definir las siguientes funciones:**

; **a) MI-FIND-IF, que tiene como argumentos un predicado p y una lista L, y devuelve el primer **elemento de L que satisface p o falso si ninguno lo satisface.**
(
    DEFUN MI-FIND-IF (P L)
        (find P L)
)
(format t "~%~%MI-FIND-IF | El resultado es:")
(print(MI-FIND-IF 1 '(3 1 4 5 6 2)))


; **b) MI-EVERY, que tiene como argumentos un predicado p y una lista L, y devuelve falso si algún **elemento de L no satisface p o verdadero en otro caso.**

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

; **c) MI-COUNT-IF, que tiene como argumentos un predicado p y una lista L, y devuelve el número **de elementos de L que satisfacen p.**
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


; 1. **Implementar las siguientes funciones:**

; **a) FUGA-DE-VOCALES, que tiene como argumento una cadena c y devuelve la cadena obtenida **sustituyendo en c cada vocal por un *: **(FUGA-DE-VOCALES “AeifghoU”) => “***fgh**”**
(defun fuga-de-vocales (cadena)  
    (cond ((find cadena "aeiou":test #'char-equal) #\*)  
    ((alpha-char-p cadena) cadena))) 

(print (map 'string #'fuga-de-vocales "AeifghoU"))

; **b) RESTAR-CADENAS, que tiene dos cadenas c1, c2 como argumentos, y devuelve la cadena obtenida **quitando de c2 todas las apariciones como** **subcadena** **de la cadena c2: **(RESTAR-CADENAS “ABCXYBAZABC” “AB”) => (CXYBAZC)**
(
    defun restar-cadenas (rem-string full-string &key from-end (test #'eql) 
                  test-not (start1 0) end1 (start2 0) end2 key) 
  
    (let ((subst-point (search rem-string full-string  
                            :from-end from-end 
                            :test test :test-not test-not 
                            :start1 start1 :end1 end1 
                            :start2 start2 :end2 end2 :key key))
    )

    (if subst-point 
        (concatenate 'string 
                    (subseq full-string 0 subst-point) 
                    (subseq full-string (+ subst-point (length rem-string)))) 
        full-string)
    )
) 
(print(restar-cadenas "adiós" "primero se dice hola y luego se dice adiós"))
