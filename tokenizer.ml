type token =
  | IDENTIFIER of string
  | KEYWORD of string
  | BOOLEAN of bool
  | BOOL_OP of string
  | ARITH_OP of string
  | INT_CONST of string
  | COMPARISON_OP of string
  | STRING_CONST of string
  | PAREN of char
  | ERROR_1 of string
  | ERROR of char
  | COMMA 

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '`' || c = '&' || c = '|' || c = '_' || c = '\'' || (c >= '0' && c <= '9');;

let is_digit c = c >= '0' && c <= '9' ;;

let is_bool c = (c = "true") || (c = "false") ;;

let is_boolOp c = (c = "&&") || (c = "||") || (c = "and") || (c = "or") || (c = "not") || (c = "!");;

let starts_with_char_or_underscore s =
  match String.length s with
  | 0 -> false
  | _ -> 
    let first_char = String.get s 0 in
    first_char = '_' || (first_char >= 'a' && first_char <= 'z') ;;

let is_num s = 
  let rec is_valid_dig = function
    | c when (c >= '0' && c <= '9') -> true
    | _ -> false
  in
  let rec check_digs_for_identifier = function
    | [] -> true
    | c :: cs -> is_valid_dig c && check_digs_for_identifier cs
  in
  check_digs_for_identifier (List.of_seq (String.to_seq s)) ;;


let is_identifier s =
  let rec is_valid_char = function
    | c when (c >= 'a' && c <= 'z') || (c>='A' && c<= 'Z') || (c >= '0' && c <= '9') || c = '`' || c = '_' -> true
    | _ -> false
  in
  let rec check_chars_for_identifier = function
    | [] -> true
    | c :: cs -> is_valid_char c && check_chars_for_identifier cs
  in
  starts_with_char_or_underscore s && check_chars_for_identifier (List.of_seq (String.to_seq s)) ;;

let is_word s =
  let rec is_valid_char = function
    | c when (c >= 'a' && c <= 'z') || (c>='A' && c<= 'Z') || (c >= '0' && c <= '9') || c = '`' || c = '_' || c = '&' || c = '|' -> true
    | _ -> false
  in
  let rec check_chars = function
    | [] -> true
    | c :: cs -> is_valid_char c && check_chars cs
  in
  starts_with_char_or_underscore s && check_chars (List.of_seq (String.to_seq s)) ;;

let is_keyword = function
| "if" | "else" | "then" | "pair" | "fst" | "snd" | "let" | "type" -> true
| _ -> false ;;

let rec is_char = function
  | c when is_letter c || is_digit c || c = '`' || c = '\\' || c = '_' -> true
  | _ -> false


let rec helper1 acc = function
| c :: cs when is_char c ->
  helper1 (acc ^ Char.escaped c) cs
| cs -> (acc, cs)


let rec helper2 acc = function
    | c :: cs when is_digit c -> helper2 (acc ^ Char.escaped c) cs
    | ' ' :: cs -> (int_of_string acc, cs)
    | cs -> (int_of_string acc,cs)


let rec tokenize input =
  match input with
  | [] -> []
  | c :: cs when is_letter c ->
    let (word, rest) = helper1(Char.escaped c) cs in
    if is_bool word then
      BOOLEAN (word = "true") :: tokenize rest
    else if is_boolOp word then
      BOOL_OP word :: tokenize rest
    else if is_keyword word then
      KEYWORD word :: tokenize rest
    else if is_identifier word then
      IDENTIFIER word :: tokenize rest
    else if is_num word then
      INT_CONST word :: tokenize rest
    else 
      ERROR_1 word :: tokenize rest
  (* | c :: cs when is_digit c ->
    let (number, rest) = helper2 (Char.escaped c) cs in
    INT_CONST number :: tokenize rest *)
  | '+' :: cs -> ARITH_OP "+" :: tokenize cs
  | '-' :: cs -> ARITH_OP "-" :: tokenize cs
  | '*' :: cs -> ARITH_OP "*" :: tokenize cs
  | '/' :: cs -> ARITH_OP "/" :: tokenize cs
  | '^' :: cs -> ARITH_OP "^" :: tokenize cs
  | '=' :: cs -> COMPARISON_OP "=" :: tokenize cs
  | '<' :: '=' :: cs -> COMPARISON_OP "<=" :: tokenize cs
  | '>' :: '=' :: cs -> COMPARISON_OP ">=" :: tokenize cs
  | '<' :: cs -> COMPARISON_OP "<" :: tokenize cs
  | '>' :: cs -> COMPARISON_OP ">" :: tokenize cs
  | '(' :: cs -> PAREN '(' :: tokenize cs
  | ')' :: cs -> PAREN ')' :: tokenize cs
  | '!' :: cs -> BOOL_OP "!" :: tokenize cs
  | ',' :: cs -> COMMA :: tokenize cs
  | '"' :: cs ->
    let rec consume_string acc = function
      | '"' :: rest -> (acc, rest)
      | c :: rest -> consume_string (acc ^ Char.escaped c) rest
      | [] -> ("error",[])
    in
    let (str, rest) = consume_string "" cs in
    STRING_CONST str :: tokenize rest
  | ' ' :: cs | '\n' :: cs | '\t' :: cs -> tokenize cs
  | x :: cs -> ERROR x :: tokenize cs

let tokenize_input input =
  tokenize (List.of_seq (String.to_seq input))

let print_token = function
  | IDENTIFIER s -> Printf.printf "'%s': identifier \n" s
  | KEYWORD s -> Printf.printf "'%s': keyword \n" s
  | BOOLEAN b -> Printf.printf "'%b': boolean \n" b
  | BOOL_OP s -> Printf.printf "'%s': boolean operation \n" s
  | ARITH_OP op ->
    begin
      match op with
      | "+" -> Printf.printf "'%s': arithmetic operator (PLUS) \n" op
      | "-" -> Printf.printf "'%s': arithmetic operator (SUB) \n" op
      | "*" -> Printf.printf "'%s': arithmetic operator (MUL) \n" op
      | "/" -> Printf.printf "'%s': arithmetic operator (DIV) \n" op
      | "^" -> Printf.printf "'%s': arithmetic operator (POW) \n" op
      | r -> Printf.printf "'%s': Error- Invalid Token! \n" r
    end
  | INT_CONST n -> Printf.printf "'%s': constant \n" n
  | COMPARISON_OP op -> Printf.printf "'%s': comparison operator \n" op
  | STRING_CONST s -> 
    begin
      match s with 
      | "error" -> Printf.printf "Error- Unterminated String! \n" 
      | _ -> Printf.printf "'%s': string constant \n" s

    end
  | PAREN c -> Printf.printf "'%c': parenthesis \n" c
  | COMMA -> Printf.printf "',': comma \n"
  | ERROR_1 r -> Printf.printf "'%s': Error- Invalid Token! \n" r
  | ERROR c -> Printf.printf " '%c' : Error- Invalid Token! \n" c



let input_string () =
  let input_line = read_line () in
  String.trim input_line
  (* let () =
  print_string "Enter the expression you want to teokenize: ";
  let entered_string = input_string () in
  let tokens = tokenize_input entered_string in
  let () = List.iter print_token tokens in
  print_newline();; *)


let rec main counter =
  if counter > 0 then begin
    print_string (string_of_int (20-counter+1) ^ ". Enter the expression you want to tokenize " ^  ": ");
    let entered_string = input_string () in
    let tokens = tokenize_input entered_string in
    List.iter print_token tokens;
    print_newline ();
    
    main (counter - 1)
  end

let () = main 20;;
(* EXHAUSTIVE TEST CASES  *)

(* 

A. IDENTIFIERS

1. Enter the expression you want to tokenize : x
'x': identifier 

2. Enter the expression you want to tokenize : anup    
'anup': identifier 

3. Enter the expression you want to tokenize : _you
'_you': identifier 

4. Enter the expression you want to tokenize : _Pro
'_Pro': identifier 

5. Enter the expression you want to tokenize : _touR
'_touR': identifier 

6. Enter the expression you want to tokenize : tmiP
'tmiP': identifier 

7. Enter the expression you want to tokenize : g_i
'g_i': identifier 

8. Enter the expression you want to tokenize : f`
'f`': identifier 

9. Enter the expression you want to tokenize : fp_s`
'fp_s`': identifier 

10. Enter the expression you want to tokenize : _gh`
'_gh`': identifier 

INCORRECT IDENTIFIERS

1. Enter the expression you want to tokenize : 8or
'8or': Error- Invalid Token! 

2. Enter the expression you want to tokenize : Ayt
'Ayt': Error- Invalid Token! 

3. Enter the expression you want to tokenize : Bli
'Bli': Error- Invalid Token! 

4. Enter the expression you want to tokenize : ]ui
 ']' : Error- Invalid Token! 
'ui': identifier 

5. Enter the expression you want to tokenize : @art
 '@' : Error- Invalid Token! 
'art': identifier 

B. KEYWORDS

1. Enter the expression you want to tokenize : let
'let': keyword 

2. Enter the expression you want to tokenize : if
'if': keyword 

3. Enter the expression you want to tokenize : then
'then': keyword 

4. Enter the expression you want to tokenize : else
'else': keyword 

5. Enter the expression you want to tokenize : pair
'pair': keyword 

6. Enter the expression you want to tokenize : fst
'fst': keyword 

7. Enter the expression you want to tokenize : snd
'snd': keyword 

8. Enter the expression you want to tokenize : type
'type': keyword   

C. BOOLEANS 

1. Enter the expression you want to tokenize : true
'true': boolean 

2. Enter the expression you want to tokenize : false
'false': boolean 

3. Enter the expression you want to tokenize : &&
'&&': boolean operation 

4. Enter the expression you want to tokenize : ||
'||': boolean operation 

5. Enter the expression you want to tokenize : and
'and': boolean operation 

6. Enter the expression you want to tokenize : or
'or': boolean operation 

7. Enter the expression you want to tokenize : not
'not': boolean operation

NUMBERS and ARITHMETIC OPERATIONS

1. Enter the expression you want to tokenize : 0
'0': constant 

2. Enter the expression you want to tokenize : 19
'19': constant 

3. Enter the expression you want to tokenize : 3+4
'3': constant 
'+': arithmetic operator (PLUS) 
'4': constant 

4. Enter the expression you want to tokenize : let x = 5-6*7
'let': keyword 
'x': identifier 
'=': comparison operator 
'5': constant 
'-': arithmetic operator (SUB) 
'6': constant 
'*': arithmetic operator (MUL) 
'7': constant 

5. Enter the expression you want to tokenize : 6/6
'6': constant 
'/': arithmetic operator (DIV) 
'6': constant 

6. Enter the expression you want to tokenize : let variance = (3+4)/3
'let': keyword 
'variance': identifier 
'=': comparison operator 
'(': parenthesis 
'3': constant 
'+': arithmetic operator (PLUS) 
'4': constant 
')': parenthesis 
'/': arithmetic operator (DIV) 
'3': constant 

7. Enter the expression you want to tokenize : let pow = x^2
'let': keyword 
'pow': identifier 
'=': comparison operator 
'x': identifier 
'^': arithmetic operator (POW) 
'2': constant 


COMPARISON OPERATORS

1. Enter the expression you want to tokenize : a > b
'a': identifier 
'>': comparison operator 
'b': identifier 

2. Enter the expression you want to tokenize : let x = if 2 >= 3 then x1 else x2
'let': keyword 
'x': identifier 
'=': comparison operator 
'if': keyword 
'2': constant 
'>=': comparison operator 
'3': constant 
'then': keyword 
'x1': identifier 
'else': keyword 
'x2': identifier 

3. Enter the expression you want to tokenize : let greater a b = if a>b then true else false
'let': keyword 
'greater': identifier 
'a': identifier 
'b': identifier 
'=': comparison operator 
'if': keyword 
'a': identifier 
'>': comparison operator 
'b': identifier 
'then': keyword 
'true': boolean 
'else': keyword 
'false': boolean 

STRINGS

1. Enter the expression you want to tokenize : "RESULT"
'RESULT': string constant 

2. Enter the expression you want to tokenize : let st = "Hello, World!"
'let': keyword 
'st': identifier 
'=': comparison operator 
'Hello, World!': string constant

3. Enter the expression you want to tokenize : "yo!       "(<- this quotation
 mark is not actually present in input, i have to put it 
because ocaml doesnt allow unterminated string literals in comments)   
Error- Unterminated String! 


COMMAS AND PARENTHESIS

1. Enter the expression you want to tokenize : a,b,c
'a': identifier 
',': comma 
'b': identifier 
',': comma 
'c': identifier 

2. Enter the expression you want to tokenize : (a,b),c
'(': parenthesis 
'a': identifier 
',': comma 
'b': identifier 
')': parenthesis 
',': comma 
'c': identifier 

3. Enter the expression you want to tokenize : (a)
'(': parenthesis 
'a': identifier 
')': parenthesis 

4. Enter the expression you want to tokenize : (89)
'(': parenthesis 
'89': constant 
')': parenthesis 

5. Enter the expression you want to tokenize : (6,7,42)
'(': parenthesis 
'6': constant 
',': comma 
'7': constant 
',': comma 
'42': constant 
')': parenthesis 

GENERAL EXPRESSIONS :




*)
