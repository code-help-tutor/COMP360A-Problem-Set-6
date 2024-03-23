WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
open OUnit2
open Printf
open Ps6.Solvers

(*---------------------------------------------------------------*)
(*                        Test helpers                           *)
(*---------------------------------------------------------------*)

let string_of_option (string_of : 'a -> string) (a : 'a option) : string =
  match a with
  | None -> "None"
  | Some a1 -> string_of a1
  
let string_of_list (string_of : 'a -> string) (l : 'a list) : string =
  let rec aux (l : 'a list) : string =
    match l with
    | [] -> ""
    | x :: [] -> string_of x
    | x :: xs -> sprintf "%s %s" (string_of x) (aux xs)
  in
  sprintf "[%s]" (aux l)

let string_of_literal (l : literal) : string =
  match l with
  | Pos s -> s
  | Neg s -> sprintf "¬%s" s

let string_of_mapping (p : string * bool) : string =
  let (s, b) = p in sprintf "%s=%s" s (string_of_bool b) 

let string_of_literal_list (a : literal list) : string =
  string_of_list string_of_literal a

let string_of_clause_opt (c : clause option) : string =
  string_of_option string_of_literal_list c

let string_of_cnf (f : cnf) : string =
  string_of_list (string_of_list string_of_literal) f

let string_of_clause_list_opt (a : clause list option) : string =
  string_of_option string_of_cnf a

let string_of_assignment (a : assignment) : string =
  string_of_list string_of_mapping a

let string_of_assignment_opt (a : assignment option) : string =
  string_of_option (string_of_list string_of_mapping) a


let check_equal_literal_list a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_literal_list

let check_equal_clause_opt a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_clause_opt

let check_equal_cnf a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_cnf

let check_equal_clause_list = check_equal_cnf

let check_equal_clause_list_option a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_clause_list_opt

let check_equal_assignment a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_assignment

let check_equal_assignment_opt a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_assignment_opt

let check_equal_string a1 a2 _ =
  assert_equal a1 a2 ~printer:(fun s -> s)

let check_equal_bool a1 a2 _ =
  assert_equal a1 a2 ~printer:string_of_bool

(*---------------------------------------------------------------*)
(*                            Tests                              *)
(*---------------------------------------------------------------*)

let suite =
  "test">:::
    [
      "example test">::
        check_equal_literal_list
          []
          []
    ;
      "example test">::
        check_equal_clause_opt
          None
          None
    ;
      "example test">::
        check_equal_cnf
          []
          []
    ;
      "example test">::
        check_equal_clause_list
          []
          []
    ;
      "example test">::
        check_equal_clause_list_option
          None
          None
    ;
      "example test">::
        check_equal_assignment
          []
          []
    ;
      "example test">::
        check_equal_assignment_opt
          None
          None
    ;
      "example test">::
        check_equal_string
          ""
          ""
    ;
      "example test">::
        check_equal_bool
          true
          true
    ;
      "apply_empty">::
        check_equal_cnf
          [[]]
          (apply_empty [[Pos "x"]; []])
    ]
    
let () =
  run_test_tt_main suite
