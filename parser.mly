%{
  let group_id = ref 1
%}

%token EOF

/* Operator */
%token RANGE

/* Pairs */
%token NLBRACKET LBRACKET RBRACKET
%token PLOOKAHEAD NLOOKAHEAD PLOOKBEHIND NLOOKBEHIND NONCAPTURING LPARENTHESIS RPARENTHESIS

/* Quantifiers */
%token <int * int> FROMTO
%token <int> FROM EXACTLY
%token LAZYMAY LAZYMUST LAZYANY

/* Text */
%token <char> CHAR
%token HSPACE NONHSPACE
%token VSPACE NONVSPACE
%token WHITESPACE NONWHITESPACE
%token DIGIT NONDIGIT
%token WORDCHAR NONWORDCHAR
%token NEWLINE NONNEWLINE
%token ANYCHAR
%token BACKREF

/* Entrypoint */
%start start
%type <AST.t> start

%%

start:
  | main_expr EOF { $1 }

main_expr:
  | main_atom quantifier           { [($1, $2)] }
  | main_atom quantifier main_expr { ($1, $2)::$3 }

quantifier:
  | FROMTO   { From_to $1 }
  | FROM     { From $1 }
  | EXACTLY  { Exactly $1 }
  | LAZYMAY  { Lazy_may }
  | LAZYMUST { Lazy_must }
  | LAZYANY  { Lazy_any }
  |          { Exactly 1 }

special_char:
  | HSPACE        { Special "hspace" }
  | NONHSPACE     { Special "nonhspace" }
  | VSPACE        { Special "vspace" }
  | NONVSPACE     { Special "nonvspace" }
  | WHITESPACE    { Special "whitespace" }
  | NONWHITESPACE { Special "nonwhitespace" }
  | DIGIT         { Special "digit" }
  | NONDIGIT      { Special "nondigit" }
  | WORDCHAR      { Special "wordchar" }
  | NONWORDCHAR   { Special "nonwordchar" }
  | NEWLINE       { Special "newline" }
  | NONNEWLINE    { Special "nonnewline" }
  | ANYCHAR       { Special "_" }

string_atom:
  | CHAR         { Regular $1 }
  | special_char { $1 }

main_atom:
  | string_atom { $1 }
  | one         { $1 }
  | group       { $1 }

one:
  | LBRACKET one_expr RBRACKET  { One_of one_expr }
  | NLBRACKET one_expr RBRACKET { None_of one_expr }

one_atom:
  | string_atom                   { $1 }
  | string_atom RANGE string_atom { Range ($1, $2) }

one_expr:
  | one_atom          { [$1] }
  | one_atom one_expr { $1::$2 }

group:
  | PLOOKAHEAD main_expr RPARENTHESIS   { Look_ahead main_expr }
  | NLOOKAHEAD main_expr RPARENTHESIS   { Negative_look_ahead main_expr }
  | PLOOKBEHIND main_expr RPARENTHESIS  { Look_behind main_expr }
  | NLOOKBEHIND main_expr RPARENTHESIS  { Negative_look_behind main_expr }
  | NONCAPTURING main_expr RPARENTHESIS { No_capture main_expr }
  | LPARENTHESIS main_expr RPARENTHESIS { let id = !group_id in (group_id := id + 1; Capture (id, main_expr))}
