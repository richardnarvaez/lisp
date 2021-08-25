;;Problema 2.6 Empleando únicamente CAR, CDR y CONS como funciones para manejo de listas,
; definir recursivamente funciones que resuelvan los siguientes problemas: 

    ;a)  Dados una lista de símbolos L y un símbolo s, encontrar la lista originada a partir de L suprimiendo
    ; todas las apariciones de s en L. 
    ;; ----------------------------------------------------------------------------
    ( defun supr (lst s)
        ( cond 
            ((null lst) nil)
            ((eql (car lst) s) (supr (cdr lst) s))
            (T (cons (car lst) (supr (cdr lst) s)))
        )
    )

    (format t "~%2.6 A | El resultado es:")
    (print(supr '(1 2 3 1 5 1 6 7 88 5) 1))
    ;; ----------------------------------------------------------------------------

    ; b)  Dados una lista de símbolos L y dos símbolos sv, sn, encontrar la lista originada a partir
    ; de L sustituyendo todas las apariciones de sv por sn. 

    ;; ----------------------------------------------------------------------------
    ( defun sust (lst sv sn)
        ( cond
            ((null lst) nil)
            ((eql (car lst) sv) (cons sn (sust (cdr lst) sv sn)))
            (T (cons (car lst) (sust (cdr lst) sv sn)))
        )
    )
    (format t "~%~%2.6 B | El resultado es:")
    (print(sust '(1 2 3) 3 4))
    ;; ----------------------------------------------------------------------------

    ; c) Dados una lista L -posiblemente con anidamientos- y un símbolo s, encontrar la lista originada a partir de L suprimiendo
    ; todas las apariciones de s en L a cualquier profundidad. 
    ;; ----------------------------------------------------------------------------
    ( defun supr_anid (lst s)
        ( cond
            ((null lst) nil)
            ((eql (car lst) s) (supr_anid (cdr lst) s))
            ((atom (car lst)) (cons (car lst) (supr_anid (cdr lst) s)))
            (T (cons (supr_anid (car lst) s) (supr_anid (cdr lst) s)))
        )
    )
    (format t "~%~%2.6 C | El resultado es:")
    (print(supr_anid '((1 2) 3 4) 1))
    ;; ----------------------------------------------------------------------------

    ;;d) Dados una lista L -posiblemente con anidamientos- y un símbolo s, 
    ; encontrar la lista originada a partir de L suprimiendo todas las apariciones de s en L a cualquier profundidad.
    ;; ----------------------------------------------------------------------------
    ( defun sust_anid (lst sv sn)
        ( cond
            ((null lst) nil)
            ((eql (car lst) sv) (cons sn (sust_anid (cdr lst) sv sn)))
            ((atom (car lst)) (cons (car lst) (sust_anid (cdr lst) sv sn)))
            (T (cons (sust_anid (car lst) sv sn) (sust_anid (cdr lst) sv sn)))
        )
    )
    (format t "~%~%2.6 D | El resultado es:")
    (sust_anid '((1 2) 3 4) 1 0)
    ;; ----------------------------------------------------------------------------

;;Problema 2.7Empleando las macros ya conocidas, pero únicamente las funciones +, NULL, EQL, ATOM, CAR, CDR y CONS,
; definir recursivamente las siguientes funciones: 
    ;; ----------------------------------------------------------------------------
    ( defun element (a lst)
        ( cond
            ((null lst) nil)
            ((eql a (car lst)) t)
            (t (element a (cdr lst)))
        )
    )
    ;; ----------------------------------------------------------------------------

    ;;a)una función que cuente el número de átomos diferentes que aparecen en L, suponiendo que L es una lista de átomos.
    ;; ---------------------------------------------------------------------------- 
    ( defun count_1 (lst)
        ( cond
            ((null lst) 0)
            ((element (car lst) (cdr lst)) (count_1 (cdr lst)))
            (T (+ 1 (count_1 (cdr lst))))
        )
    )
        (format t "~%~%2.7 A | El resultado es:")
        (print(count_1 '(1 1 2 3 3)))
    ;; ----------------------------------------------------------------------------

    ;;b)una función que cuente el número de átomos diferentes que aparecen en L a cualquier nivel,
    ; suponiendo que L puede tener anidamientos. 
    ;; ----------------------------------------------------------------------------
    ( defun count_2 (lst)
        (count_1 (flatten lst))
    )
    ( defun flatten (lst)
        ( cond
            ((null lst) nil)
            ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
            (T (concat_lists (flatten (car lst)) (flatten (cdr lst))))
        )
    )
    ( defun concat_lists (l1 l2)
        ( cond 
            ((null l1) l2)
            (T (cons (car l1) (concat_lists (cdr l1) l2)))
        )
    )
    (format t "~%~%2.7 B | El numero de atomos diferentes es:")
    (print(count_2 '((1 2 2) 1 3 3 4 (9 8 9))))
    ;; ----------------------------------------------------------------------------
    ;;c)una función que devuelva una lista que tenga exactamente una aparición de cada elemento de L.
    ( defun remove_dup (lst)
        ( cond
            ((null lst) nil)
            ((element (car lst) (cdr lst)) (remove_dup (cdr lst)))
            (t (cons (car lst) (remove_dup (cdr lst))))
        )
    )
    (format t "~%~%2.7 C | Despues de quitar todos los elementos:")
    (print(remove_dup '(1 1 2 3 3)))


;;Problema 2.8 Los polinomios de Chebychev Tn(x) se definen como sigue: 
; T0(x) = 1 
; T1(x) = x 
; Tn+1(x) = 2x 
; Tn(x) - Tn-1(x) 

    ;;a) diseñar una representación de los polinomios, con los correspondientes constructores y selectores. 
    ;; ----------------------------------------------------------------------------
    ( defun mult (pol p esc) 
        ( cond
            ((null pol) nil)
            ((> p 0) (cons 0 (mult pol (- p 1) esc)))
            (T (cons (* esc (car pol)) (mult (cdr pol) p esc)))
        )
    )

    ( defun sum (pol1 pol2)
        ( cond
            ((null pol1) pol2)
            ((null pol2) pol1)
            (T (cons (+ (car pol1) (car pol2)) (sum (cdr pol1) (cdr pol2))))
        )
    )
    ;; ----------------------------------------------------------------------------

    ;;b) escribir en LISP una función recursiva CHEBY(n) que devuelva el n-ésimo polinomio de Chebychev. 
    ;; ----------------------------------------------------------------------------
    ( defun CHEBY (n)
        ( cond
            ((= n 0) '(1))
            ((= n 1) '(0 1))
            (T (sum (mult (CHEBY (- n 1)) 1 2) (mult (CHEBY (- n 2)) 0 -1)))
        )
    )

    (format t "~%~%2.8 | El n-esimo polinomio Cheby:")
    (print(CHEBY 5))
    ;; ----------------------------------------------------------------------------

;;Problema 2.9 Si se supone que la pertenencia de un elemento a un conjunto no es un valor 
; booleano ("verdadero" o "falso"), sino un grado que puede ser cualquier número real en 
; [0, 1], se tiene lo que se denomina un conjunto difuso. Por definición, si e pertenece a A con 
; grado g1 y a B con grado g2, e pertenece a A∪B con grado max(g1, g2) y e pertenece a A∩B con grado min(g1, g2).
; Diseñar una representación de los conjuntos difusos basada en listas, definiendo para ella la unión y la intersección
    ;; ----------------------------------------------------------------------------
    ( defun grado (C e)
        ( cond
            ((null C) 0)
            ((eql (car (car C)) e) (car (cdr (car C))))
            (T (grado (cdr C) e))
        )
    )
    ( defun element2 (A e)
        ( cond
            ((null A) nil)
            ((eql (caar A) e))
            (T (element2 (cdr A) e))
        )
    )
    ( defun remover (A e)
        ( cond
            ((null A) nil)
            ((eql (caar A) e) (remover (cdr A) e))
            (T (cons (car A) (remover (cdr A) e)))
        )
    )
    ;;Funcion union
    ( defun my_union (A B)
        ( cond
            ((null A) B)
            ((element2 B (caar A)) ( let
                ((g1 (grado A (caar A))) (g2 (grado B (caar A))))
                (cons (list (caar A) (MAX g1 g2)) (my_union (cdr A) (remover B (caar A))))
            ))
            (T (cons (car A) (my_union (cdr A) B)))
        )
    )
    ;;Funcion intersection
    ( defun my_intersection (A B)
        ( cond
            ((null A) nil)
            ((element2 B (caar A)) ( let
                ((g1 (grado A (caar A))) (g2 (grado B (caar A))))
                (cons (list (caar A) (MIN g1 g2)) (my_intersection (cdr A) (remover B (caar A))))
            ))
            (T (my_intersection (cdr A) B))
        )
    )
    (format t "~%~%2.9 | Union:")
    (print(my_union '((1 0.5) (3 1)) '((2 0.3) (3 0.7))))
    (format t "~%~%2.9 | Interseccion:")
    (print(my_intersection '((1 0.5) (3 1)) '((2 0.3) (3 0.7))))
    ;; ----------------------------------------------------------------------------


;;Problema 2.10 Una proposición lógica es una proposición atómica o una proposición compuesta. 
; Las proposiciones compuestas son 
; -la negación de una proposición 
; -la disyunción de dos o más proposiciones 
; -la conjunción de dos o más proposiciones 
; Se pide:
    ; a) representar las proposiciones mediante listas, con los correspondientes constructores y selectores. 
    ; b)definir la función VALOR-VERDAD, que devuelve el valor de verdad de una proposición. 
    ; c)supongamos que también se desean representar las implicaciones y equivalencias. Repetir los apartados a) y b). 

    ;; ----------------------------------------------------------------------------
    ( defun element3 (P e)
        ( cond
            ((null P) nil)
            ((null e) T)
            ((listp e) (AND (element3 P (car e)) (element3 P (cdr e))))
            ((atom (car P)) (if (equal (string (car P)) (string e)) T (element3 (cdr P) e)))
            (T (if (element3 (car P) e) T (element3 (cdr P) e)))
        )
    )
    ( defun create_prop (P)
        ( cond
            ((null P) NIL)
            ((listp (car P)) (concatenate 'string (create_prop (car P)) (create_prop (cdr P))))
            ((equal (car P) "~") (concatenate 'string "~" (create_prop (cdr P))))
            ((equal (car P) "|") (concatenate 'string (string (cadr P)) (create_prop_and_or (cddr P) "|")))
            ((equal (car P) "&") (concatenate 'string (string (cadr P)) (create_prop_and_or (cddr P) "&")))
            ((equal (car P) "->") (concatenate 'string (create_prop (list (cadr P))) "->" (create_prop (list (caddr P)))))
            ((equal (car P) "<->") (concatenate 'string (create_prop (list (cadr P))) "<->" (create_prop (list (caddr P)))))
            ((atom (car P)) (concatenate 'string (string (car P)) (create_prop (cdr P))))
        )
    )
    ( defun create_prop_and_or (P s)
        ( cond 
            ((null P) nil)
            (T (concatenate 'string s (create_prop (list (car P))) (create_prop_and_or (cdr P) s)))
        )
    )

    ( defun valor_verdad (P prop)
        ( cond 
            ((null P) nil)
            ((element3 (list (car P)) prop) (create_prop (list (car P))))
            (T (valor_verdad (cdr P) prop))
        )
    )
    (format t "~%~%2.10 | Valor de de verdad:")
    (print(valor_verdad '("A" ("->" "B" ("~" C)) ("<->" "D" ("|" "E" "F" "G"))) '("->" "B")))
    ;; ----------------------------------------------------------------------------