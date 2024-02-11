type token =
  | IDENTIFIER of string
  | KEYWORD of string
  | BOOLEAN of bool
  | BOOL_OP of string
  | ARITH_OP of string
  | INT_CONST of int
  | COMPARISON_OP of string
  | STRING_CONST of string
  | PAREN of char
  | ERROR_1 of string
  | ERROR
  | COMMA 

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '\'' || c = '&' || c = '|';;

let is_digit c = c >= '0' && c <= '9' ;;

let is_bool c = (c = "true") || (c = "false") ;;

let is_boolOp c = (c = "&&") || (c = "||");;

let starts_with_char_or_underscore s =
  match String.length s with
  | 0 -> false
  | _ -> 
    let first_char = String.get s 0 in
    first_char = '_' || (first_char >= 'a' && first_char <= 'z') ;;


let is_identifier s =
  let rec is_valid_char = function
    | c when (c >= 'a' && c <= 'z') || (c>='A' && c<= 'Z') || (c >= '0' && c <= '9') || c = '\'' || c = '_' -> true
    | _ -> false
  in
  let rec check_chars_for_identifier = function
    | [] -> true
    | c :: cs -> is_valid_char c && check_chars_for_identifier cs
  in
  starts_with_char_or_underscore s && check_chars_for_identifier (List.of_seq (String.to_seq s)) ;;

let is_word s =
  let rec is_valid_char = function
    | c when (c >= 'a' && c <= 'z') || (c>='A' && c<= 'Z') || (c >= '0' && c <= '9') || c = '\'' || c = '_' || c = '&' || c = '|' -> true
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
  | c when is_letter c || is_digit c || c = '\'' || c = '\\' || c = '_' -> true
  | _ -> false


let rec helper1 acc = function
| c :: cs when is_char c ->
  helper1 (acc ^ Char.escaped c) cs
| cs -> (acc, cs)


let rec helper2 acc = function
    | c :: cs when is_digit c -> helper2 (acc ^ Char.escaped c) cs
    | cs -> (int_of_string acc, cs)


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
    else 
      ERROR_1 word :: tokenize rest
  | c :: cs when is_digit c ->
    let (number, rest) = helper2 (Char.escaped c) cs in
    INT_CONST number :: tokenize rest
  | '+' :: cs -> ARITH_OP "+" :: tokenize cs
  | '-' :: cs -> ARITH_OP "-" :: tokenize cs
  | '*' :: cs -> ARITH_OP "*" :: tokenize cs
  | '/' :: cs -> ARITH_OP "/" :: tokenize cs
  | '=' :: cs -> COMPARISON_OP "=" :: tokenize cs
  | '<' :: '=' :: cs -> COMPARISON_OP "<=" :: tokenize cs
  | '>' :: '=' :: cs -> COMPARISON_OP ">=" :: tokenize cs
  | '<' :: cs -> COMPARISON_OP "<" :: tokenize cs
  | '>' :: cs -> COMPARISON_OP ">" :: tokenize cs
  | '(' :: cs -> PAREN '(' :: tokenize cs
  | ')' :: cs -> PAREN ')' :: tokenize cs
  | ',' :: cs -> COMMA :: tokenize cs
  | '"' :: cs ->
    let rec consume_string acc = function
      | '"' :: rest -> (acc, rest)
      | c :: rest -> consume_string (acc ^ Char.escaped c) rest
      | [] -> raise (Failure "Unterminated string literal")
    in
    let (str, rest) = consume_string "" cs in
    STRING_CONST str :: tokenize rest
  | ' ' :: cs | '\n' :: cs | '\t' :: cs -> tokenize cs
  | _ :: cs -> ERROR :: tokenize cs

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
      | _ -> failwith "Invalid arithmetic operator"
    end
  | INT_CONST n -> Printf.printf "'%d': constant \n" n
  | COMPARISON_OP op -> Printf.printf "'%s': comparison operator \n" op
  | STRING_CONST s -> Printf.printf "'%s': string constant \n" s
  | PAREN c -> Printf.printf "'%c': parenthesis \n" c
  | COMMA -> Printf.printf "',': comma \n"
  | ERROR_1 r -> Printf.printf "'%s': Error- Invalid Token! \n" r
  | ERROR -> Printf.printf " Error- Invalid Token! \n"

(* Example usage *)
let input = " x1' + 42 * (y - 3) = (true || false) && pair"
let tokens = tokenize_input input
let () = List.iter print_token tokens;;

print_newline();;
