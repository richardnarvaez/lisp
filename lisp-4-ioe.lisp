; P.6.7. 
; a) Definir ? como un carácter macro, de manera que ?expresión1 expresión2 se lea como
; expresión2:
; >?PEPE '(A B C)
; (A B C)
(SET-MACRO-CHARACTER '? (lambda (x) (list x)))

; b) Ciertas aplicaciones MS-DOS escriben uno o varios ^Z al final del archivo. Sin embargo, READ no
; acepta este carácter como nombre de símbolo, ni lo reconoce como marca de final de archivo. Definir
; ^Z como carácter macro de forma que se evite este problema.
(SET-MACRO-CHARACTER '^Z (lambda (x) (list x)))

; P.6.8. a) Definir ! como un carácter macro de envío y R como un subcarácter, de manera que
; !Rcadena se lea como el número cuya notación romana es cadena:
; >!RXVIII
; 18

(defun mapcn (chars nums string)
    (loop as char across string as i = (position char chars)collect (and i (nth i nums))))

(defun parse-roman (R)
    (loop with nums = (mapcn "IVXLCDM" '(1 5 10 50 100 500 1000) R)
        as (A B) on nums if A sum (if (and B (< A B)) (- A) A)))

(dolist (r '("MCMXC" "MDCLXVI" "MMVIII")) 
    (format t "~a:~10t~d~%" r (parse-roman r)))

; b) Definir ! como un carácter macro de envío e I como un subcarácter, de manera que !Icadena se
; lea como el contenido del archivo cuyo nombre es cadena (se supone que el contenido del archivo es
; una sola expresión Lisp).

(defun roman-numeral (n) 
    (format nil "~@R" n))

(roman-numeral 2)

; P.6.9. a) Implementar versiones robustas de las funciones para el manejo de polígonos de R.2.8, de
; manera que señalen los errores adecuados cuando reciban argumentos que no sean de tipo correcto.
; Inteligencia Artificial e I.C.. 2002/2003 6.27 Dpto. Leng. Ciencias Comp.
; b) Implementar versiones de las mismas funciones, de forma que los errores sean recuperables,
; proporcionando al usuario la posibilidad de cambiar los argumentos proporcionados.

(
    DEFUN FACT-PROTEGIDO (X Y L)
    (
        COND  
        ((NOT (NUMBERP X)) (ERROR "~S no es un numero." X))
        ((NOT (INTEGERP X)) (ERROR "~S no es un numero." X))
        ((NOT (NUMBERP Y)) (ERROR "~S no es un numero." Y))
        ((NOT (INTEGERP Y)) (ERROR "~S no es un numero." Y))
        ((NOT (NUMBERP L)) (ERROR "~S no es un numero." L))
        ((NOT (INTEGERP L)) (ERROR "~S no es un numero." L))
        (T (LIST 'TRIANGULO X Y L))
    )
)

(FACT-PROTEGIDO 'M '3.3 'x)


(
    DEFUN ESNUMERO (S)  
        (ASSERT (AND (NUMBERP S) (INTEGERP S) 
        (OR (ZEROP S) (PLUSP S))) (S) "~S no es un numero." S) T)
)

(
    DEFUN FACT-PROTEGIDO (X Y L)
    (
        COND  
        ((EQL (ESNUMERO X) T))
        ((EQL (ESNUMERO Y) T))
        ((EQL (ESNUMERO L) T))
        (T (LIST 'TRIANGULO X Y L))
    )
)

(FACT-PROTEGIDO 'P '4.3 '1)

; P.6.10. a) Implementar una versión de EVALQUOTE que procese adecuadamente las formas
; especiales.
; b) Implementar una versión de EVALQUOTE que desprecie los errores aritméticos y los de
; entrada/salida.

(
    defun tokenize-stream (stream) 
        (labels ((whitespace-p (char)
                    (find char #(#\space #\newline #\return #\tab))
                )
                (consume-whitespace ()
                    (loop while (whitespace-p (peek-char nil stream nil #\a))
                        do (read-char stream)
                    )
                )
                (read-integer ()
                    (loop while (digit-char-p (peek-char nil stream nil #\space))
                    collect (read-char stream) into digits
                    finally (return (parse-integer (coerce digits 'string)))
                    )
                )
            )
            (consume-whitespace)
            (let* ((c (peek-char nil stream nil nil)))
                (token (case c
                    (nil nil) (#\( :lparen) (#\) :rparen) (#\* '*) (#\/ '/) (#\+ '+) (#\- '-)
                        (otherwise
                            (unless (digit-char-p c)
                                (cerror "Skip it." "Unexpected character ~w." c)
                                (read-char stream)
                                (return-from tokenize-stream
                                    (tokenize-stream stream))
                            )
                            (read-integer)
                        )
                    )
                )
            )
            (unless (or (null token) (integerp token))
            (read-char stream))
            token)
 )

(
    defun evaluate (string) 
        (with-input-from-string (in string)
            (eval
            (infix-to-prefix
                (group-operations
                    (group-parentheses (loop for token = (tokenize-stream in)
                    until (null token) collect token)
                    )
                )
            ))
        )
)

(evaluate "1 - 5 * 2 / 20 + 1")