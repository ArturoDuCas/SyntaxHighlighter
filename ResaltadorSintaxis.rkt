#lang racket

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
      [(regexp-match? #px"^-?\\d+$" token)  "integer"]
      [(regexp-match? #px"^[0-9]+\\.[0-9]*$" token) "float"]
      [(regexp-match? #px"^\".*\"$" token) "string"]
      [(regexp-match? #px"^\'.*\'$" token) "string"]
      [(regexp-match? #px"^#.*n*$" token) "comment"]
      [(regexp-match? #px"^[+\\-*//%=<>!&|^]$" token) "operator"]
      [(regexp-match? #px"^[\\[\\]\\{\\}\\(\\)]+$" token) "delimeter"]
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



;; Ejemplo de uso
;Escribimos las reglas de css y las etiquetas de apertura de html
(write-text (filestart))
;Aqui debería ir el analisis token por token del archivo
(token_writer "#Esto es un comentario")
(token_writer "\"Esto es un sring\"")
(token_writer "variable_123")
(token_writer "+")
(token_writer "False")
;Escribimos las etiquetas de cierre de html
(token_writer (fileclose))
