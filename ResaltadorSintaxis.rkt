#lang racket
(require racket/base)
(require racket/string)

(define (verificar-palabra-reservada? str)
  (define palabras-reservadas
    '("False" "None" "True" "and" "as" "assert" "async" "await" "break" "class"
      "continue" "def" "del" "elif" "else" "except" "finally" "for" "from"
      "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
      "raise" "return" "try" "while" "with" "yield"))
  (member str palabras-reservadas))

(define (filestart)
  "<!DOCTYPE html>
<html>
  <head>
    <style>

        .comment {
          color: green;
        }

        .var {
          color: lightskyblue;
        }
        
        .reserved {
          color: purple;
        }
        
        .method {
            color: red;
        }
        
        .literal {
          color: lightgreen;
        }
        
        .string {
          color: orange;
        }

        .operator {
          color: yellow;
        }
        
        .delimiter {
          color: yellow;
        }
        
        .error {
            color: red;
            text-decoration: red wavy underline; 
        }
        
        html {
          background-color: black;
          line-height: .1px;
        }

        p {
            display: inline-block;
        }
    </style>
  </head>
  <body>"
  )

(define (fileclose)
"
</body>
</html>
"
  )

;Escribir cualquier cosa en el archivo
(define output-file "output.html") ; Nombre del archivo de salida

(define (write-text texto)
  (with-output-to-file output-file
    #:mode 'text
    #:exists 'append
    (lambda ()
      (write-string texto))))

;Escribir una etiqueta para cada token
(define (write-tag token class)
  (write-text "<p class=\"")
  (write-text class)
  (write-text "\">")
  (write-text token)
  (write-text "</p>")
  )

(define (token_writer token)
  ;Identificamos el tipo de token
  (define (token_evaluator token)
    (cond
      [(verificar-palabra-reservada? token) "reserved" ]
      [(regexp-match? #px"^[a-zA-Z][_a-zA-Z0-9]*$" token)  "var" ]
      [(regexp-match? #px"^-?\\d+$" token)  "literal"]
      [(regexp-match? #px"^[0-9]+\\.[0-9]*$" token) "literal"]
      [(regexp-match? #px"^\".*\"$" token) "string"]
      [(regexp-match? #px"^\'.*\'$" token) "string"]
      [(regexp-match? #px"^#.*n*$" token) "comment"]
      [(regexp-match? #px"^[+\\-*//%=<>!&|^]$" token) "operator"]
      [(regexp-match? #px"^[\\[\\]\\{\\}\\(\\)]+$" token) "delimiter"]
      ["error"]
      ))

  ;Escribir una etiqueta para cada token
  (define (write-tag token class)
    (write-text "<p class=\"")
    (write-text class)
    (write-text "\">")
    (write-text token)
    (write-text "</p>")
    )
  
  (write-tag token (token_evaluator token))
)

; Imprimir un salto de linea
(define (write_break)
  (write-text "<br>")
  )

; Imprimir un whitespace
(define (write_space)
  (write-text "<p>&nbsp&nbsp;<p>")
  )




; read-line ->  procedimiento para leer una línea del archivo file.
; 'any -> Indica que cualquier valor válido (incluido el EOF, es decir, el final del archivo) es aceptado como una línea.
; begin -> Para agrupar múltiples expresiones en secuencia y especificar que todas las expresiones deben evaluarse en orden
(define (readFilePerLine file)
  (let ((line (read-line file 'any)))
    (if (not (eof-object? line))
      (begin
       (readLinePerSpaces line)
       (readFilePerLine file)
       )
      (displayln "FIN ARCHIVO")
     )
   )
 )

; Funcion recibe un string y regresa el string sin el primer caracter
(define (removeFirstChar str)
      (substring str 1 (string-length str))
 )

; Funcion recibe unstring y regresa un string del primer caracter
(define (getFirstChar str)
      (string (string-ref str 0))
  )


(define (readLinePerSpaces line)
  (define (readLinePerSpaces_tr line word_ac isComment)
    (cond
      [isComment 
                    (if (> (string-length line) 0)
                        (readLinePerSpaces_tr (removeFirstChar line) (string-append word_ac (getFirstChar line)) #t)
                        (
                         (token_writer word_ac)
                         (write_break)
                         )
                     )
                    
       ]
      [#t  
(if (> (string-length line) 0)
         (
          if (not (string=? (getFirstChar line) " "))
                                            ( 
                                              if (string=? (getFirstChar line) "#")
                                                (begin
                                                   (token_writer word_ac)
                                                   (readLinePerSpaces_tr (removeFirstChar line) "#" #t)
                                                 )
                                                 (
                                                     readLinePerSpaces_tr (removeFirstChar line) (string-append word_ac (getFirstChar line)) #f
                                                  )
                                             )


                                       (begin ; Si el caracter es un espacio
                                         (
                                           if (> (string-length word_ac) 0)
                                              ( ; Si el acumulador esta cargado
                                                 begin 
                                                 (token_writer word_ac)
                                                 (write_space)
                                               )

                                              ( ; Si el acumulador no tiene nada
                                                  write_space
                                               )
                                         )

                                         (readLinePerSpaces_tr (removeFirstChar line) "" #f)
                                       )
                                       )
         
                                       
         (begin
          (token_writer word_ac)
          (write_break)
          )

             )
  ]
      )

    



          )
  (readLinePerSpaces_tr line "" #f)
)






;Escribimos las reglas de css y las etiquetas de apertura de html
(write-text (filestart))


; call-with-input-file -> procedimiento que toma dos argumentos: el nombre del archivo a abrir y la función a ejecutar. e encarga de abrir el archivo, pasar el objeto de archivo a la función y asegurarse de que el archivo se cierre adecuadamente una vez que se completa la ejecución de la función.
(call-with-input-file "InputFile.txt" readFilePerLine)


;Escribimos las etiquetas de cierre de html
(token_writer (fileclose))
