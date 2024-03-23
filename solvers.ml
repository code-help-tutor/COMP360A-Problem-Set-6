WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
type literal =
  | Pos of string
  | Neg of string

type clause = literal list
type cnf = clause list

type assignment = (string * bool) list


let apply_clause (c : clause) (s : string) (b : bool) : clause option =
  (* [apply_clause c s b] applies the assignment { s = b } to the
     clause c *)
  ignore c; ignore s; ignore b; None (* Implement this function correctly *)


let apply_empty (f : cnf) : cnf =
  (* [apply_empty f] applies the empty assignment to the cnf
     formula f *)
  if List.mem [] f then [[]] else f


let apply_sing (f : cnf) (s : string) (b : bool) : cnf =
  (* [apply_sing f s b] applies the assignment { s = b } to the
     formula f *)
  ignore f; ignore s; ignore b; [] (* Implement this function correctly *)


let rec apply (f : cnf) (a : assignment) : cnf =
  (* [apply f a] applies the assignment a to the formula f *)
  match a with
  | [] -> apply_empty f
  | (s, b) :: a1 -> let f1 = apply_sing f s b in apply f1 a1


let assignment_of_literals (ll : literal list) : assignment =
  (* [assignment_of_literals ll] turns a list of literals into an
     assigment by mappping variables in positive literals to 1 and
     variables in negative literals to 0 *)
  List.map (fun l -> match l with | Pos s -> s, true | Neg s -> s, false) ll


let rec choose_var (f : cnf) : string =
  (* [choose_var f] returns the first variable in f, and raises an
     exception if none is found. *)
  match f with
  | [] -> invalid_arg "Input contains no literals."
  | [] :: f1 -> choose_var f1
  | (Pos s :: _) :: _ -> s
  | (Neg s :: _) :: _ -> s


let naive_sat (f : cnf) : assignment option =
  (* [naive_sat f] returns a satisfying assignment for f if there is one,
     using naive backtracking *)
  ignore f; None (* Implement this function correctly *)


let find_unit_clauses (f : cnf) : literal list =
  (* [find_unit_clauses f] returns the literals appearing in unit
     clauses *)
  ignore f; [] (* Implement this function correctly *)


let find_pure (f : cnf) : literal list =
  (* [find_pure f] returns the literals which are pure in f *)
  ignore f; [] (* Implement this function correctly *)


let dpll (f : cnf) : assignment option =
  (* [dpll f] returns a satisfying assignment for f if there is one,
     using the DPLL algorithm:

     1. check if f is the empty formula; return satisfying assignment
     if so;

     2. check if f contains the empty clause; return None if so;

     3. find the unit clauses in f, and if any are found, recur on the
     result of applying the corresponding assignment to f;

     4. find the pure literals in f, and if any are found, recur on
     the result of applying the corresponding assignment to f;

     5. choose a variable x, try satisfying f{x = 1}; failing
     that, try satisfying f{x = 0}
   *)
  ignore f; None (* Implement this function correctly *)


let proper_sublist (f : 'a -> bool) (l : 'a list) : 'a list option =
  (* [proper_sublist f l] filters l with the predicate f, and returns
     a list only if [f a] is false for some element a of l *)
    let rec aux (l : 'a list) : ('a list, 'a list) result =
      match l with
      | [] -> Error []
      | a :: l1 when f a ->
         (match aux l1 with
          | Error l3 -> Error (a :: l3)
          | Ok l3 -> Ok (a :: l3))
      | _ :: l1 ->
         (match aux l1 with
          | Error l3 -> Ok l3
          | Ok l3 -> Ok l3)
    in
    Result.to_option (aux l)


let subset (l1 : 'a list) (l2 : 'a list) : bool =
  (* [subset l1 l2] checks if every member of l1 is a member of l2 *)
  List.for_all (fun a -> List.mem a l2) l1


let is_taut (c : clause) : bool =
  (* [is_taut c] determines if c is a tautology *)
  ignore c; true (* Implement this function correctly *) 


let complement (l : literal) : literal =
  (* [complement l] returns the complement of l *)
  match l with
  | Pos s -> Neg s
  | Neg s -> Pos s


let resolve_on (l : literal) (c1 : clause) (c2 : clause) : clause option =
  (* [resolve_on l c1 c2] attempts to resolve c1 and c2 under the
     assumption that l appears in c1 and the complement of l appears
     in c2; returns None also if the resolvent is a tautology *)
  ignore l; ignore c1; ignore c2; None (* Implement this function correctly *) 
  

let resolve (c1 : clause) (c2 : clause) : clause list =
  (* [resolve c1 c2 acc] attempts to resolve c1 and c2 in every way
     possible, returning the non-tautological resolvents  *)
  ignore c1; ignore c2; [] (* Implement this function correctly *) 


let resolve_pairwise (cl1 : clause list) (cl2 : clause list) : clause list =
  (* [resolve_pairwise cl1 cl2] attempts to resolve every clause in
     cl1 with every clause in cl2 in every way possible, and returns
     non-tautological resolvents *)
  ignore cl1; ignore cl2; [] (* Implement this function correctly *) 


let naive_ref (f : cnf) : (clause list) option =
  (* [naive_ref f] returns a refutation if f is unsatisfiable and
     None otherwise. Note that the refutation is ordered so that
     parent clauses appear after their resolvents. Algorithm:

     1. check if f contains the empty clause: if so, return the
     refutation [[]].

     2. we initially let clause lists cl1 and cl2 both be f;
     
     3. find all possible non-tautological resolvents of a clause in
     cl1 with a clause in cl2: we let the clause list rl be this
     list of resolvents;

     4. if rl contains the empty clause, append [] to cl1 and return
     the result as a refutation;

     5. if rl is a subset of cl1, return None;
     
     6. Otherwise, let clause lists cl1 and cl2 be, respectively, rl
     appended to cl1 and rl, and goto step 3.
   *)
  ignore f; None (* Implement this function correctly *) 
