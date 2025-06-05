{
open Lexing
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let string = [^ '1' '(' ')' '|' '\\' '.' ',' '-' '<' '>' '+' '*' ':' '=' ' ' '\t' '\r' '\n']+

rule token = parse
  | eof { EOF }
  | white { token lexbuf }
  | "1" { UNIT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "|" { PIPE }
  | "\\" { BACKSLASH }
  | "." { DOT }
  | "," { COMMA }
  | "->" { RIGHTARROW }
  | "<->" { BIARROW }
  | "+" { PLUS }
  | "*" { TIMES }
  | ":" { COLON }
  | "=" { EQUAL }
  | "injl" { INJL }
  | "injr" { INJR }
  | "fold" { FOLD }
  | "let" { LET }
  | "in" { IN }
  | "iso" { ISO }
  | "end" { END }
  | "fix" { FIX }
  | "type" { TYPE }
  | "invert" { INVERT }
  | "program" { PROGRAM }
  | "fl" { FOLDINJL }
  | "fr" { FOLDINJR }
  | "mu" { MU }
  | string { ID (lexeme lexbuf) }

