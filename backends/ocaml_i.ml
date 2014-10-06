open Compiler
open Parser
open Lexer

(* TODO : Working nested environment inside code environment *)

type interpreter = { out : out_channel; inp : in_channel }

type env = {target: string ; ocaml : interpreter; mutable seq: bool } 

let send_write env s = 
	output_string env.ocaml.out s

let send env s = 
	env.seq <- false ; send_write env s

let trim s=
	let len = String.length s - 1 in 
	let rec forward n s = if n >= len then n else
	match s.[n] with 
		| ' ' | '#' -> forward (n+1) s
		| _ -> n 
	in
	let n = forward 0 s in
	String.sub s n (len-n+1)   

let read_ocaml env =
 let input = ref "" in
while !input <> "Exception: End_of_file."  do
(* for i=1 to 2 do *) 
	if !input <> "- : unit = ()" && !input <> "\n" then Printf.printf "%s\n" !input;
	let s = trim @@ input_line env.ocaml.inp in
	input:= s
done

let flush_i env =  (* send env "raise End_of_file;;\n"; *) flush env.ocaml.out (*; read_ocaml env *)

let eval env =  send env ";;\n" ; flush_i env; env.seq <- false 


let preambule env = let c = Printf.sprintf
"
module Hydra = struct 
let f = open_out \"%s\" 

let context = ref []

let write s= match !context with
  |[] -> output_string f s
  | b :: q -> Buffer.add_string b s

let new_context ()= 
  let b=Buffer.create 80 in
  context := b:: !context  

let pop_context () = match !context with
  | [] -> ()
  | b::q -> context:=q; Buffer.contents b |> write
end
"
env.target in
send env c; eval env 



let write env s=  if env.seq then send_write env ";"  else env.seq <- true ;  send_write env @@ "Hydra.write ("^s^")\n"
let write_text env s=  write env ("\"" ^ s ^"\"")  

let init_env basename = 
	let target  = basename
	and inp, out =   (*open_in "ocaml.inpt", open_out "ocaml.out"*)  Unix.open_process "ocaml" in
	{ target ; ocaml = {out;inp}; seq=false }




let write_text env kind s = 
	match kind with
	| Text -> write_text env s ; eval env 
	| Code -> send env s
	| Capture -> write_text env s
	| Inclusion -> write env s 
			
let write_node env kind hydres k = match kind with
	| Code -> k env hydres ; eval env 
	| Capture ->  send env "begin\nHydra.new_context ()\n"; env.seq<-true; k env hydres;  send env "; Hydra. pop_context ()\nend\n"  
	| _ -> k env hydres
			 
let end_compilation env = send env "flush Hydra.f"; eval env ; ignore ( Unix.close_process (env.ocaml.inp,env.ocaml.out) )
  

let backend = transcompile {
	init_env; 
	preambule;
	write_node;
	write_text;
	end_compilation
}

