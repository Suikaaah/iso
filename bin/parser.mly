%{
open TypesTop
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
%type <(string * (string * base_type) list) list> ts
%type <(string * base_type) list> arms
%type <string * base_type> arm
%type <value> value
%type <pattern> pattern
%type <expr> expr
%type <pairs> pairs
%type <iso> iso
%type <term> term
%%

program:
  | ts = ts; EQUAL; t = term; EOF; { { ts; t } }

ts:
  | TYPE; x = ID; EQUAL; arms = arms; rest = ts; { (x, arms) :: rest }
  | PROGRAM; { [] }

arms:
  | arm = arm; PIPE; rest = arms; { arm :: rest }
  | arm = arm; { [arm] }

arm:
  | c = ID; OF; a = base_type; { (c, a) }
  | c = ID; { (c, Unit) }

value:
  | LPAREN; RPAREN; { Unit }
  | c = ID; v = value; { Constructed { c; v } }
  | x = ID; { Variable x }
  | LPAREN; v_1 = value; COMMA; v_2 = value; RPAREN; { Pair (v_1, v_2) }

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

