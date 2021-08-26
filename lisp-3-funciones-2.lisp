; P.4.6. Deseamos diseñar un programa para representar y manejar cubos y tetraedros, definidos
; ambos por las coordenadas de su centro y la longitud de su arista. El programa debe ser capaz de
; calcular los volúmenes y las superficies de estas figuras.
; a)Definir una implementación Lisp basada en el paso de mensajes, en la que los objetos se
; representen como cierres léxicos.
; b) Modificar la implementación dada en a) para representar también las esferas. Cada esfera se
; identifica por su radio y las coordenadas de su centro.
; P.4.7. Repetir el ejercicio P.4.6, empleando esta vez el paradigma de programación basada en el
; paso de mensajes. 

(DEFUN CUUBO (X Y L)
    #'(LAMBDA (MSJ)
    (CASE MSJ (LADO L)
        (CENTROX X)
        (CENTROY Y)
        (ALTURA (* 0.5 (SQRT 3) L))
        (AREA (* 0.5 L (* 0.5 (SQRT 3) L)))
        (PERIMETRO (* 3 L)))))

(DEFUN TETRAEDRO (X Y L)
    #'(LAMBDA (MSJ)
        (CASE MSJ (LADO L)
        (CENTROX X)
        (CENTROY Y)
        (ALTURA L)
        (AREA (* L L))
        (PERIMETRO (* 4 L)))))


