type expression = 
  | Integer of int
  | Var of string
  | Add of expression * expression
  | Sub of expression * expression
  | Lambda of string * expression
  | Let of string * expression * expression
  | App of expression * expression

and instruction = 
  | IConst of int
  | IAdd
  | ISub
  | IAccess of int
  | IVarFree of int
  | IGrab
  | IPush of code

and value = 
  | VVarFree of int
  | VInt of int
  | VClosure of code * env
  | VCode of code
  | VEnv of env

and env = value list
and code = instruction list

(* and machine = int list * instr list *)

type dvar = 
  | Free of int
  | Bounded of int

type dterm = 
  | DInt of int
  | DVar of dvar
  | DAbs of dterm
  | DApp of dterm * dterm
  | DAdd of dterm * dterm
  | DSub of dterm * dterm

module Env = Map.Make(struct type t = string let compare = String.compare end)

let dterm_of_term envb term =
  let envf = ref Env.empty in
  let rec dterm_of_term envb = function
  | Integer n -> DInt n
  | Add (e1, e2) -> DAdd (dterm_of_term envb e1, dterm_of_term envb e2)
  | Sub (e1, e2) -> DSub (dterm_of_term envb e1, dterm_of_term envb e2)
  | Var s ->
      if Env.mem s envb then
	(DVar (Bounded (Env.find s envb)))
      else
	if Env.mem s !envf then
	  (DVar (Free (Env.find s !envf)))
	else
	  begin
	    let n = Env.cardinal !envf in
	      envf := Env.add s n !envf ;
	      (DVar (Free n))
	  end 
  | Lambda (s, t) -> 
      DAbs (dterm_of_term (Env.add s 0 (Env.map succ envb)) t)
  | App (t1, t2) -> 
      DApp (dterm_of_term envb t1, dterm_of_term envb t2)
  | Let (v, t1, l) ->
      DApp (DAbs (dterm_of_term (Env.add v 0 (Env.map succ envb)) l), 
	    dterm_of_term envb t1)
  in
    dterm_of_term envb term

let rec compile = function
  | DInt n -> [IConst n]
  | DVar (Bounded n) -> [IAccess n]
  | DVar (Free n) -> [IVarFree n]
  | DAbs e -> [IGrab] @ (compile e)
  | DApp (e1, e2) -> [IPush (compile e2)] @ (compile e1)
  | DAdd (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
  | DSub (e1, e2) ->  (compile e1) @ (compile e2) @ [ISub]

(*  | Let (s, e1, e2) -> (compile e1) @ [ILet] @ (compile e2) @ [IEndLet] *)

let rec eval (instructions, env, stack) =
  match instructions with
    | [] -> List.hd stack
    | IConst n :: rest -> eval (rest, env, VInt(n) :: stack)
    | IAdd :: rest ->
	begin
	  match stack with
	    | VInt(n2) :: VInt(n1) :: stack -> 
		eval (rest, env, VInt(n1 + n2) :: stack)
	    | _ -> failwith "Add needs two arguments"
	end
    | ISub :: rest ->
	begin
	  match stack with
	    | VInt(n2) :: VInt(n1) :: stack -> 
		eval (rest, env, VInt(n1 - n2) :: stack)
	    | _ -> failwith "Sub needs two arguments"
	end
    | IAccess n :: rest ->
	begin
	  match  (List.nth env n) with
	    | VClosure (c', e') ->
		eval (c', e',  stack)
	    | _ -> failwith "IAccess"
	end
    | IVarFree n :: rest ->
	eval (rest, env, VVarFree n :: stack)
    | IGrab :: c ->
	begin
	  match stack with
	    | [] ->
		VClosure (instructions, env)
	    | v :: s ->
		eval (c, v :: env, s)
	end
    | IPush (c') :: c -> 
	eval (c, env, VClosure (c', env) :: stack)

let rec output_instruction out_chan = function
  | IConst n -> output_byte out_chan 0 ; output_binary_int out_chan n
  | IAdd -> output_byte out_chan 1
  | ISub -> output_byte out_chan 2
  | IAccess n -> output_byte out_chan 3 ; output_binary_int out_chan n
  | IVarFree n -> output_byte out_chan 9 ; output_binary_int out_chan n
  | IPush code -> output_byte out_chan 10 ; List.iter (output_instruction out_chan) code ; output_byte out_chan 11
  | IGrab -> output_byte out_chan 12

and input_push in_chan =
  let rec input_push accu = 
    match input_byte in_chan with
      | 0 -> let n = input_binary_int in_chan in
	  input_push (IConst n :: accu)
      | 1 -> input_push (IAdd :: accu)
      | 2 -> input_push (ISub :: accu)
      | 3 -> let n = input_binary_int in_chan in
	  input_push (IAccess n :: accu)
      | 9 -> let n = input_binary_int in_chan in
	  input_push (IVarFree n :: accu)
      | 10 -> let code = input_push [] in
	  input_push (IPush code :: accu)
      | 11 -> List.rev accu
      | 12 -> input_push (IGrab :: accu)
      | _ -> failwith "Unkown opcode"
  in
    input_push []

and input_instruction in_chan = 
  match input_byte in_chan with
    | 0 -> IConst (input_binary_int in_chan)
    | 1 -> IAdd
    | 2 -> ISub
    | 3 -> IAccess (input_binary_int in_chan)
    | 9 -> IVarFree (input_binary_int in_chan)
    | 10 -> IPush (input_push in_chan)
    | 12 -> IGrab
    | _ -> failwith "Unkown opcode"

let rec string_of_expression = function
  | Integer n -> string_of_int n
  | Var s -> s
  | Add (e1, e2) -> "(" ^ (string_of_expression e1) ^ " + " ^ 
      (string_of_expression e2) ^ ")"
  | Sub (e1, e2) -> "(" ^ (string_of_expression e1) ^ " - " ^ 
    (string_of_expression e2) ^ ")"
  | Lambda (s, e) -> "fun " ^ s ^ " -> " ^ (string_of_expression e)
  | Let (s, e1, e2) -> "let " ^ s ^ " = " ^ (string_of_expression e1) ^ " in " ^ 
      (string_of_expression e2)
  | App (e1, e2) -> (string_of_expression e1) ^ " " ^ (string_of_expression e2)

let rec string_of_instruction = function
  | IConst n -> "CONST " ^ (string_of_int n)
  | IAdd -> "ADD"
  | ISub -> "SUB"
  | IAccess n -> "ACCESS " ^ (string_of_int n)
  | IVarFree n -> "VARFREE " ^ (string_of_int n)
  | IPush code -> "PUSH (" ^ (string_of_instructions code) ^ ")"
  | IGrab -> "GRAB"

and string_of_instructions = function
  | [] -> ""
  | instruction :: [] ->
      (string_of_instruction instruction)
  | instruction :: rest -> 
      (string_of_instruction instruction) ^ " " ^ (string_of_instructions rest)

let rec string_of_value = function
  | VVarFree n -> string_of_int n
  | VInt n -> string_of_int n
  | VClosure (code, env) -> 
      "CLOSURE " ^ (string_of_instructions code)
  | VCode code -> string_of_instructions code
  | VEnv env -> string_of_values env

and string_of_values = function
  | [] -> ""
  | value :: rest ->
      (string_of_value value) ^ " " ^ (string_of_values rest)

let rec string_of_dterm = function
  | DInt n -> " " ^ (string_of_int n) ^ " "
  | DAdd (e1, e2) -> 
      " (" ^ (string_of_dterm e1) ^ " + " ^ (string_of_dterm e2) ^ ") "
  | DSub (e1, e2) -> 
      " (" ^ (string_of_dterm e1) ^ " - m" ^ (string_of_dterm e2) ^ ") "
  | DVar (Free n) -> " F(" ^ (string_of_int n) ^ ") "
  | DVar (Bounded n) -> " B(" ^ (string_of_int n) ^ ") "
  | DAbs (t) -> " (fun -> " ^ (string_of_dterm t) ^ ") "
  | DApp (t1, t2) -> 
      " (" ^ (string_of_dterm t1) ^ " " ^ (string_of_dterm t2) ^ ") "
