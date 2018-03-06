open GT       
open Language

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

let evalInstruction config instr = match config, instr with
  | (y::x::stack, config), BINOP op -> ((Language.Expr.binop op x y)::stack, config)
  | (stack, config), CONST z -> (z::stack, config)
  | (stack, (state, z::inp, out)), READ -> (z::stack, (state, inp, out))
  | (z::stack, (state, inp, out)), WRITE -> (stack, (state, inp, out @ [z]))
  | (stack, (state, inp, out)), LD x -> (state x)::stack, (state, inp, out)
  | (z::stack, (state, inp, out)), ST x -> (stack, ((Language.Expr.update x z state), inp, out))


(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)

let rec eval config prog = match config, prog with
  | config, instr::prog -> eval (evalInstruction config instr) prog
  | config, [] -> config

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile_expr = function
  | Language.Expr.Const num -> [CONST num]
  | Language.Expr.Var var -> [LD var]
  | Language.Expr.Binop (op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP op]

let rec compile = function
  | Language.Stmt.Read x -> [READ; ST x]
  | Language.Stmt.Write e -> compile_expr e @ [WRITE]
  | Language.Stmt.Assign (x, e) -> compile_expr e @ [ST x]
  | Language.Stmt.Seq (s1, s2) -> compile s1 @ compile s2