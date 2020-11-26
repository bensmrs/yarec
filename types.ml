type quantifier_t = From_to of int * int
                  | From of int
                  | Exactly of int

type qualified_quantifier_t = Greedy of quantifier_t
                            | Lazy of quantifier_t
                            | Possessive of quantifier_t

type shorthand = H_space | Non_h_space
               | V_space | Non_v_space
               | White_space | Non_white_space
               | Digit | Non_digit
               | Word_char | Non_word_char
               | New_line | Non_new_line
               | Any_char

type one_atom_t = Shorthand of shorthand
                | Single of char
                | Range of char * char

type main_atom_t = Special of shorthand
                 | Regular of char
                 | One_of of one_atom_t list
                 | None_of of one_atom_t list
                 | Look_ahead of t
                 | Negative_look_ahead of t
                 | Look_behind of t
                 | Negative_look_behind of t
                 | No_capture of t
                 | Capture of int * t
                 | Back_ref of int

and quantified_t = Quantified of main_atom_t * qualified_quantifier_t
                 | Start_of_line
                 | End_of_line

and main_expr_t = quantified_t list

and t = Expr of main_expr_t
      | Either of main_expr_t * t
