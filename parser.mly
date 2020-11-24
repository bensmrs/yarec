%{
  open Types
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
  | main_expr EOF { $1 }

quantified:
  | STARTL                         { Start_of_line }
  | ENDL                           { End_of_line }
  | a=main_atom qq=qualified_quantifier { let qf,ql = qq in Quantified (a, qf, ql) }

main_expr:
  | quantified           { [$1] }
  | quantified main_expr { $1::$2 }

qualified_quantifier:
  | quantifier            { Greedy, $1 }
  | quantifier LAZY       { Lazy, $1 }
  | quantifier POSSESSIVE { Possessive, $1 }

quantifier:
  | FROMTO   { let (start, stop) = $1 in From_to (start, stop) }
  | FROM     { From $1 }
  | EXACTLY  { Exactly $1 }
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
  | BACKREF     { Back_ref $1 }

one:
  | LBRACKET one_expr RBRACKET  { One_of $2 }
  | NLBRACKET one_expr RBRACKET { None_of $2 }

one_atom:
  | CHAR            { Single $1 }
  | CHAR RANGE CHAR { Range ($1, $3) }

one_expr:
  | one_atom          { [$1] }
  | one_atom one_expr { $1::$2 }

group:
  | PLOOKAHEAD main_expr RPARENTHESIS   { Look_ahead(Positive,$2) }
  | NLOOKAHEAD main_expr RPARENTHESIS   { Look_ahead(Negative,$2) }
  | PLOOKBEHIND main_expr RPARENTHESIS  { Look_behind(Positive,$2) }
  | NLOOKBEHIND main_expr RPARENTHESIS  { Look_behind(Negative,$2) }
  | NONCAPTURING main_expr RPARENTHESIS { No_capture $2 }
  | LPARENTHESIS main_expr RPARENTHESIS { let id = !group_id in (group_id := id + 1; Capture (id, $2))}
