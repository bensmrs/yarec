%{
  open Types
  let group_id = ref 1
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
%token HSPACE NONHSPACE
%token VSPACE NONVSPACE
%token WHITESPACE NONWHITESPACE
%token DIGIT NONDIGIT
%token WORDCHAR NONWORDCHAR
%token NEWLINE NONNEWLINE
%token ANYCHAR
%token STARTL ENDL
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
  | main_atom qualified_quantifier { Quantified ($1, $2) }

main_expr:
  | quantified           { [$1] }
  | quantified main_expr { $1::$2 }

top_expr:
  | main_expr             { Expr $1 }
  | main_expr OR top_expr { Either ($1, $3) }

qualified_quantifier:
  | quantifier            { Greedy $1 }
  | quantifier LAZY       { Lazy $1 }
  | quantifier POSSESSIVE { Possessive $1 }

quantifier:
  | FROMTO   { let (start, stop) = $1 in From_to (start, stop) }
  | FROM     { From $1 }
  | EXACTLY  { Exactly $1 }
  |          { Exactly 1 }

special_char:
  | HSPACE        { H_space }
  | NONHSPACE     { Non_h_space }
  | VSPACE        { V_space }
  | NONVSPACE     { Non_v_space }
  | WHITESPACE    { White_space }
  | NONWHITESPACE { Non_white_space }
  | DIGIT         { Digit }
  | NONDIGIT      { Non_digit }
  | WORDCHAR      { Word_char }
  | NONWORDCHAR   { Non_word_char }
  | NEWLINE       { New_line }
  | NONNEWLINE    { Non_new_line }
  | ANYCHAR       { Any_char }

string_atom:
  | CHAR         { Regular $1 }
  | special_char { Special $1 }

main_atom:
  | string_atom { $1 }
  | one         { $1 }
  | group       { $1 }
  | BACKREF     { Back_ref $1 }

one:
  | LBRACKET one_expr RBRACKET  { One_of ($2) }
  | NLBRACKET one_expr RBRACKET { None_of ($2) }

one_atom:
  | CHAR            { Single $1 }
  | special_char    { Shorthand $1 }
  | CHAR RANGE CHAR { Range ($1, $3) }

one_expr:
  | one_atom          { [$1] }
  | one_atom one_expr { $1::$2 }

group:
  | PLOOKAHEAD top_expr RPARENTHESIS   { Look_ahead $2 }
  | NLOOKAHEAD top_expr RPARENTHESIS   { Negative_look_ahead $2 }
  | PLOOKBEHIND top_expr RPARENTHESIS  { Look_behind $2 }
  | NLOOKBEHIND top_expr RPARENTHESIS  { Negative_look_behind $2 }
  | NONCAPTURING top_expr RPARENTHESIS { No_capture $2 }
  | LPARENTHESIS top_expr RPARENTHESIS { let id = !group_id in (group_id := id + 1; Capture (id, $2))}
