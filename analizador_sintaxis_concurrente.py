import  re
from concurrent.futures import ThreadPoolExecutor

f = open('index.html','w')

#String en el que almacenamos el inicio de nuestro documento html y las clases para cada tipo de elemento de python
FILE_START = """
<!DOCTYPE html>
<html>
  <head>
    <style>

        .comment {
          color: green;
        }

        .identifyer {
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
  <body>
"""
#string en el que guardamos las etiquetas de cierre de <html> y <body>
FILE_CLOSE = """
</body>
</html>
"""

# Receives the name of a file and returns a list with each of its lines.
def getFile(file_name):
    with open(file_name, "r") as file: 
        return file.read()

def verReservada(str):
    palabras_reservadas = ["False", "None", "True", "and", "as", "assert", "async", "await", "break", "class",
                           "continue", "def", "del", "elif", "else", "except", "finally", "for", "from",
                           "global", "if", "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass",
                           "raise", "return", "try", "while", "with", "yield"]
    return str in palabras_reservadas

def verificar_palabra_reservada(token):
    return verReservada(token) 

def check_identifyer(token):
    return "identifyer" if re.match(r'^[a-zA-Z][a-zA-Z0-9_]*$', token) else None

def check_literal(token):
    return "literal" if re.match(r'^-?\d+$', token) or re.match(r'^[0-9]+\.[0-9]*$', token) else None

def check_string(token):
    return "string" if re.match(r'^\".*\"$', token) or re.match(r'^\'.*\'$', token) else None

def check_comment(token):
    return "comment" if re.match(r'^#.*n*$', token) else None

def check_operator(token):
    return "operator" if re.match(r'^[\+\:\-\,\*\/%=!<>]$', token) else None

def check_delimiter(token):
    return "delimiter" if re.match(r'^[\(\)\[\]\{\}]$', token) else None

#Strings en los que almacenamos la etiqueta con la que se personalizará cada token de python sgún su tipo
def OpenTag(tag):
    return '<p class="' + tag + '">'

def ClosingTag():
    return '</p>'

def token_writer(token, html_class):
    tag = OpenTag(html_class) + token + ClosingTag()
    f.write(tag)

# Implementación de concurrencia
def token_evaluator(token):
    #  Utilizamos ThreadPoolExecutor para ejecutar las funciones de verificación de tokens de manera concurrente
     with ThreadPoolExecutor() as executor:
        futures = []
        
        if verificar_palabra_reservada(token):
            futures.append(executor.submit(lambda: "reserved"))
        
        futures.append(executor.submit(check_identifyer, token))
        futures.append(executor.submit(check_literal, token))
        futures.append(executor.submit(check_string, token))
        futures.append(executor.submit(check_comment, token))
        futures.append(executor.submit(check_operator, token))
        futures.append(executor.submit(check_delimiter, token))
        
        for future in futures:
            result = future.result()
            if result is not None:
                return result
        
        return "error"
    

def getToken(file_string):
    token = ""
    isString = False
    isComment = False
    for char in file_string:
        # verificación de strings
        if char == '"' and isString == False:
            isString = True
            token = token + char
            continue
        if isString == True and char != '"':
            token = token + char
            continue
        if char == '"' and isString == True:
            isString = False
            token = token + char
            token_writer(token, token_evaluator(token))
            token = ""
            continue
        
        #verificación de commentarios 
        if char == '#' and isComment == False:
            isComment = True
            token = token + char
            continue
        if isComment == True and char != "\n":
            token = token + char
            continue
        if char == '\n' and isComment == True:
            isComment = False
            token = token + char
            token_writer(token, token_evaluator(token))
            f.write("<br>")
            token = ""
            continue
        
        if char not in [" ", ":", "+", "-", "<", ">", "{", "}", "[", "]", "=", "(", ")", "," ]:
            token = token + char
        if char in [" ", ":", "+", "-", "<", ">", "{", "}", "[", "]", "=", "(", ")", ","]:
            # print("done")
            token_writer(token, token_evaluator(token))
            token = ""
            token_writer(char, token_evaluator(char))
        if char == "\n":
            f.write("<br>")
            token = token[:-1]
        if char == " ":
            f.write("<p>&nbsp&nbsp;<p>")
            # print (token)
            token_writer(token, token_evaluator(token))
            token = ""
        
        


def main():
  # file_name = input("Ingrese el nombre del archivo: ")
  file_name = "ejemplo.txt"


  f.write(FILE_START)
  
  file_name = 'ejemplo.txt'
  file = getFile(file_name) 
  getToken(file)

  f.write(FILE_CLOSE)
  f.close()
  # print(token_evaluator("vari"))


main() 