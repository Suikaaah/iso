{
open Lexing
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let string = [^ '1' '(' ')' '{' '}' '\\' '.' '-' '<' '>' '+' '*' ':' '=' ' ' '\t' '\r' '\n']+

rule token = parse
    | white { token lexbuf }
    | "1" { UNIT }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "\\" { BACKSLASH }
    | "." { DOT }
    | "->" { RIGHTARROW }
    | "<->" { BIARROW }
    | "+" { PLUS }
    | "*" { TIMES }
    | ":" { COLON }
    | "=" { EQUAL }
    | string { ID (lexeme lexbuf) }
    | newline { EOL }

