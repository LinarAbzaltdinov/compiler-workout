(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let binop op left right = 
      let 
        int2bool value = if value = 0 then false else true
        and bool2int value = if value then 1 else 0
      in
        match op with
          | "+" -> left + right
          | "-" -> left - right
          | "*" -> left * right
          | "/" -> left / right
          | "%" -> left mod right
          | ">" -> bool2int (left > right)
          | "<" -> bool2int (left < right)
          | ">=" -> bool2int (left >= right)
          | "<=" -> bool2int (left <= right)
          | "==" -> bool2int (left = right)
          | "!=" -> bool2int (left <> right)
          | "!!" -> bool2int ((int2bool left) || (int2bool right))
          | "&&" -> bool2int ((int2bool left) && (int2bool right))
          | _ -> failwith (Printf.sprintf "Undefined operator %s" op)

    let rec eval state expr = match expr with
      | Const n -> n
      | Var x -> state x
      | Binop (op, x, y) ->
        let
          left = eval state x
          and right = eval state y
        in
          binop op left right
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval config stmt = match config, stmt with
      | (st, z::inp, out), Read x -> (Expr.update x z st), inp, out
      | (st, inp, out), Write e -> st, inp, out @ [Expr.eval st e]
      | (st, inp, out), Assign (x, e) -> (Expr.update x (Expr.eval st e) st), inp, out
      | config, Seq (s1, s2) -> eval (eval config s1) s2
      | _, Read _ -> failwith "Empty input stream read"
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
