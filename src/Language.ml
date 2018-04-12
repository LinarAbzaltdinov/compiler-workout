(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty = failwith "Not implemented"

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = failwith "Not implemented"
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = failwith "Not implemented" 

    (* Creates a new scope, based on a given state *)
    let enter st xs = failwith "Not implemented"

    (* Drops a scope *)
    let leave st st' = failwith "Not implemented"

  end
    
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

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let rec eval state expr = match expr with
      | Const n -> n
      | Var x -> state x
      | Binop (op, x, y) -> binop op (eval state x) (eval state y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (
      primary: x:IDENT {Var x} | x:DECIMAL {Const x} | -"(" parse -")";
      parse: !(Ostap.Util.expr
        (fun x -> x)
        [|
          `Lefta  , [ostap ("!!"), (fun l r -> Binop("!!", l, r))];
          `Lefta  , [ostap ("&&"), (fun l r -> Binop("&&", l, r))];
          `Nona   , [
 			ostap ("<="), (fun l r -> Binop("<=", l, r));
 			ostap ("<"),  (fun l r -> Binop("<", l, r));
 			ostap (">="), (fun l r -> Binop(">=", l, r));
 			ostap (">"),  (fun l r -> Binop(">", l, r));
 			ostap ("=="), (fun l r -> Binop("==", l, r));
 			ostap ("!="), (fun l r -> Binop("!=", l, r))
          ];
          `Lefta  , [
            ostap ("+"), (fun l r -> Binop("+", l, r));
            ostap ("-"), (fun l r -> Binop("-", l, r))
          ];
          `Lefta  , [
 			ostap ("*"), (fun l r -> Binop("*", l, r));
 			ostap ("/"), (fun l r -> Binop("/", l, r));
 			ostap ("%"), (fun l r -> Binop("%", l, r))
          ]
        |]
        primary
      )
    )
    end
                    
(* Simple statements: syntax and semantics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)

    let rec eval ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x          -> (match i with z::i' -> (Expr.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e          -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)      -> (Expr.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2)    -> eval (eval conf s1) s2
      | Skip               -> conf
      | If     (e, s1, s2) -> if Expr.eval st e != 0 then eval conf s1 else eval conf s2
      | While  (e, s)      -> if Expr.eval st e != 0 then eval (eval conf s) stmt else conf
      | Repeat (e, s)      -> let (st_, i_, o_) as conf_ = eval conf s in 
                                if Expr.eval st_ e == 0 then eval conf_ stmt else conf_

    (* Statement parser *)
    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;
      stmt:
        %"read" "(" x:IDENT ")"          {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | %"skip" {Skip}
      | %"while" e:!(Expr.parse) %"do" t:parse %"od" {While (e, t)}
      | %"for" t1:parse "," e:!(Expr.parse) "," t2:parse %"do" t3:parse %"od" {Seq (t1, While (e, Seq (t3, t2)))}
      | %"repeat" t:parse %"until" e:!(Expr.parse) {Repeat (e, t)}
      | %"if" e:!(Expr.parse) %"then" t:parse 
        elifs:(%"elif" !(Expr.parse) %"then" parse)* 
        elseb:(%"else" parse)? %"fi"
        { 
          let elseBody = match elseb with
            | Some t -> t
            | None -> Skip
          in
          let newElseBody = List.fold_right (fun (e_, t_) t -> If (e_, t_, t)) elifs elseBody in
          If (e, t, newElseBody)
        }
      | x:IDENT ":=" e:!(Expr.parse) {Assign (x, e)}
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: empty {failwith "Not implemented"}
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i = failwith "Not implemented"
                                   
(* Top-level parser *)
let parse = failwith "Not implemented"
