%{
open Types
%}

%token <string> ID
%token UNIT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token BACKSLASH
%token DOT
%token COLON
%token EQUAL
%token EOL
%token RIGHTARROW
%token BIARROW
%token PLUS
%token TIMES

%right RIGHTARROW
%right BIARROW
%right PLUS
%right TIMES

%start <base_type> base_type_eol
%start <term> term_eol
%%

base_type_eol:
  | t = base_type; EOL { t }

base_type:
  | LPAREN; a = base_type; RPAREN; { a }
  | UNIT; { Unit }
  | a = base_type; PLUS; b = base_type; { Sum (a, b) }
  | a = base_type; TIMES; b = base_type; { Product (a, b) }
  | BACKSLASH; x = ID; DOT; a = base_type; { Inductive { x; a } }
  | x = ID; { Variable x }

term_eol:
  | UNIT; { Unit }
