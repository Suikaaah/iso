%{
open Types
%}

%token EOF
%token LPAREN
%token RPAREN
%token PIPE
%token BACKSLASH
%token DOT
%token COMMA
%token BIARROW
%token EQUAL
%token UNIT
%token LET
%token IN
%token ISO
%token END
%token FIX
%token TYPE
%token INVERT
%token PROGRAM
%token OF
%token <string> ID

%start <program> program
%type <(string * base_type) list> ts
%type <base_type> base_type
%type <iso_type> iso_type
%type <value> value
%type <pattern> pattern
%type <expr> expr
%type <pairs> pairs
%type <iso> iso
%type <term> term
%%

program:
  | ts = ts; COLON; a = base_type; EQUAL; t = term; EOF; { { ts; t; a } }

ts:
  | TYPE; x = ID; EQUAL; a = base_type; rest = ts; { (x, a) :: rest }
  | PROGRAM; { [] }

base_type:
  | UNIT; { Unit }
  | a = base_type; PLUS; b = base_type; { Sum (a, b) }
  | a = base_type; TIMES; b = base_type; { Product (a, b) }
  | MU; x = ID; DOT; a = base_type; { Inductive { x; a } }
  | x = ID; { Variable x }
  | LPAREN; a = base_type; RPAREN; { a }

iso_type:
  | a = base_type; BIARROW; b = base_type; { Pair (a, b) }
  | t_1 = iso_type; RIGHTARROW; t_2 = iso_type; { Arrow (t_1, t_2) }
  | LPAREN; t = iso_type; RPAREN; { t }

value:
  | LPAREN; RPAREN; { Unit }
  | x = ID; { Variable x }
  | INJL; v = value; { InjLeft v }
  | INJR; v = value; { InjRight v }
  | LPAREN; v_1 = value; COMMA; v_2 = value; RPAREN; { Pair (v_1, v_2) }
  | FOLD; v = value; { Fold v }
  | FOLDINJL; v = value; { Fold (InjLeft v) }
  | FOLDINJR; v = value; { Fold (InjRight v) }

pattern:
  | x = ID; { Variable x }
  | LPAREN; p_1 = pattern; COMMA; p_2 = pattern; RPAREN; { Pair (p_1, p_2) }

expr:
  | LET; p_1 = pattern; COLON; products = base_type; EQUAL;
    omega = iso; LPAREN; p_2 = pattern; COLON; a = base_type; RPAREN;
    IN; e = expr; { Let { p_1; omega; p_2; e; a; products } }
  | v = value; { Value v }

pairs:
  | PIPE; v = value; BIARROW; e = expr; rest = pairs; { (v, e) :: rest }
  | END; { [] }

iso:
  | ISO; p = pairs; { Pairs p }
  | FIX; phi = ID; DOT; omega = iso; { Fix { phi; omega } }
  | BACKSLASH; psi = ID; DOT; omega = iso; { Lambda { psi; omega } }
  | x = ID; { Variable x }
  | omega_1 = iso; LPAREN; omega_2 = iso; COLON; t_1 = iso_type; RPAREN;
    { App { omega_1; omega_2; t_1 } }
  | INVERT; omega = iso; { Invert omega }
  | LPAREN; omega = iso; RPAREN; { omega }

term:
  | LPAREN; RPAREN; { Unit }
  | x = ID; { Variable x }
  | INJL; t = term; { InjLeft t }
  | INJR; t = term; { InjRight t }
  | LPAREN; t_1 = term; COMMA; t_2 = term; RPAREN; { Pair (t_1, t_2) }
  | FOLD; t = term; { Fold t }
  | LET; p = pattern; COLON; products = base_type; EQUAL; t_1 = term; IN; t_2 = term;
    { Let { p; t_1; t_2; products } }
  | omega = iso; LPAREN; t = term; COLON; a = base_type; RPAREN;
    { App { omega; t; a } }
  | FOLDINJL; t = term; { Fold (InjLeft t) }
  | FOLDINJR; t = term; { Fold (InjRight t) }

