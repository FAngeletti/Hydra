open Compiler
open Parser
open Lexer

(* TODO : Working nested environment inside code environment *)

type interpreter = { out : out_channel; inp : in_channel }

type env = {target: string ; ocaml : interpreter; seq: bool; capt_args : string list } 

let send env s = 
	output_string env.ocaml.out s

let send_unit env s = 
	send env s ; { env with seq = true }

let send_partial env s = 
	send env s; {env with seq = false }

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

let eval env =  send env ";;\n" ; flush_i env; { env with seq = false } 


let preambule env = let c = Printf.sprintf
"
module Hydra = struct 
let f = open_out \"%s\" 

let write s= output_string f s

end
"
env.target in
send env c; eval env 


let hydra_write env s = 
	let (!) = send env in
	let () =   if env.seq then !";"; !"Hydra.write ("; !s; !")\n" in
	{env with seq = true }  

let escape s = "\"" ^ (String.escaped s) ^ "\""


let write_text env = function 
	| `Raw , s -> hydra_write env @@ escape s; eval env 
	| `Inclusion, s -> hydra_write env s

let write_code env = function
	| `Raw, s -> send_partial env s
	| `Inclusion, s -> hydra_write env s

let write_capture env = function
	| `Raw, s -> send_partial env @@ String.escaped s; env
	| `Inclusion, s -> let env= send_partial env "%s" in { env with capt_args = ")"::s::"("::env.capt_args }

let node_code env h k =
	let env = k env h in
	eval env

let node_capture env h k =
	let env = send_partial env "( Printf.fprintf f \"" in 
	let env = k env h in
	let () = send env "\""; send env @@ String.concat "" env.capt_args in
	let env = send_unit env ")" in
	{env with capt_args = [] } 

let init_env basename = 
	let target  = basename
	and inp, out =   (*open_in "ocaml.inpt", open_out "ocaml.out"*)  Unix.open_process "ocaml" in
	{ target ; ocaml = {out;inp}; seq=false; capt_args = [] }




			 
let end_compilation env = send env "flush Hydra.f"; eval env ; ignore ( Unix.close_process (env.ocaml.inp,env.ocaml.out) )
  

let backend = transcompile {
	init_env; 
	preambule;
	write_text;
	write_code;
	write_capture;
	node_code;
	node_capture;
	end_compilation
}

