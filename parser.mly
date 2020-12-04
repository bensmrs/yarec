%{
  open Types
%}

%token EOF

/* Operator */
%token RANGE
%token OR

/* Pairs */
%token NLBRACKET LBRACKET RBRACKET
%token PLOOKAHEAD NLOOKAHEAD PLOOKBEHIND NLOOKBEHIND NONCAPTURING LPARENTHESIS RPARENTHESIS

/* Quantifiers */
%token <int * int> FROMTO
%token <int> FROM EXACTLY
%token LAZY POSSESSIVE

/* Text */
%token <char> CHAR
%token <Types.shorthand> SPECIAL
%token STARTL ENDL
%token START END
%token <int> BACKREF

/* Entrypoint */
%start start
%type <Types.t> start

%%

start:
  | top_expr EOF { $1 }

quantified:
  | STARTL                         { Start_of_line }
  | ENDL                           { End_of_line }
  | START                          { Start_of_input }
  | END                            { End_of_input }
  | a=main_atom qq=qualified_quantifier { let qf,ql = qq in Quantified (a, qf, ql) }

main_expr:
  | quantified           { [$1] }
  | quantified main_expr { $1::$2 }

main_expr_or_nothing:
 |           { [] }
 | main_expr { $1 }

top_expr:
  | main_expr_or_nothing             { Expr $1 }
  | main_expr_or_nothing OR top_expr { Either ($1, $3) }

qualified_quantifier:
  | quantifier            { Greedy, $1 }
  | quantifier LAZY       { Lazy, $1 }
  | quantifier POSSESSIVE { Possessive, $1 }

quantifier:
  | FROMTO   { let (start, stop) = $1 in From_to (start, stop) }
  | FROM     { From $1 }
  | EXACTLY  { Exactly $1 }
  |          { Exactly 1 }

string_atom:
  | c=CHAR       { Regular c }
  | s=SPECIAL    { Special s }

main_atom:
  | string_atom { $1 }
  | one         { $1 }
  | group       { $1 }
  | BACKREF     { Back_ref $1 }

one:
  | LBRACKET one_expr RBRACKET  { One_of $2 }
  | NLBRACKET one_expr RBRACKET { None_of $2 }

one_atom:
  | CHAR            { Single $1 }
  | s=SPECIAL    { Shorthand s }
  | CHAR RANGE CHAR { Range ($1, $3) }

one_expr:
  | one_atom          { [$1] }
  | one_atom one_expr { $1::$2 }

group:
  | PLOOKAHEAD top_expr RPARENTHESIS   { Look_ahead(Positive,$2) }
  | NLOOKAHEAD top_expr RPARENTHESIS   { Look_ahead(Negative,$2) }
  | PLOOKBEHIND top_expr RPARENTHESIS  { Look_behind(Positive,$2) }
  | NLOOKBEHIND top_expr RPARENTHESIS  { Look_behind(Negative,$2) }
  | NONCAPTURING top_expr RPARENTHESIS { No_capture $2 }
  | LPARENTHESIS top_expr RPARENTHESIS { Capture $2 }
