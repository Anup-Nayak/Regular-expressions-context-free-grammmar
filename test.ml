let x = 2;;

let y = 3 + x ;;

(* print_int(y);; *)

let s = "hello";;

let starts_with_char_or_underscore s =
  match String.length s with
  | 0 -> false
  | _ -> 
    let first_char = String.get s 0 in
    first_char = '_' || (first_char >= 'a' && first_char <= 'z')

(* Example usage *)
let a = starts_with_char_or_underscore "example" (* true *)
let b = starts_with_char_or_underscore "_example" (* true *)
let c = starts_with_char_or_underscore "123" ;;(* false *)

Printf.printf "My boolean value is: %b\n" a;;
Printf.printf "My boolean value is: %b\n" b;;
Printf.printf "My boolean value is: %b\n" c;;