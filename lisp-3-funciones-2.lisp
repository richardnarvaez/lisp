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


;; P4 Definir una matriz de 10 filas y 20 columnas, rellenarla,
;; asignando a cada elemento el valor de la suma de su fila más su
;; columna y obtener una lista de salida con los elementos de la
;; diagonal.

(defvar m (make-array '(10 20)))

(prog (res)
      (dotimes (i 10)
        (dotimes (j 20)
          (setf (aref m i j) (+ i j))
           (when (= i j)
             (setf res (cons (+ i j) res)))))
      (return (reverse res)))