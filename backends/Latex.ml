open Printf
open Lexer
open Compiler
open Parser
type env = { pyx : out_channel ; ntab : int ; pygen_name : string ; latex_name: string; capt_args : string list }
let preambule env =
fprintf env.pyx 
"#!/usr/bin/env python 
#This is an automatically generated python generator file.
__latexFile__=open('%s', 'w+')

def __writeLatexFile__(str):
	__latexFile__.write(str)

def __latex__(str):
	__writeLatexFile__(str)

def __pynclusion__(obj):
	try:
		s=obj.latex()
	except AttributeError:
		s=str(obj)
	__latex__(s)

" env.latex_name ; env

let end_compilation env  = 
	close_out env.pyx;
	let error1 = Sys.command ("chmod u+x "^env.pygen_name) in
	let error2 = Sys.command ("./"^env.pygen_name) in
	match (error1,error2)  with
		| 0,0 -> ()
		| _ -> printf "Sorry something has gone wrong \n"
	


let init_env basename = 
	let pygen_name =  "gen_"^basename^".py" 
	and latex_name = basename^".tex" in 
	let pyx =pygen_name |> open_out in
	{ntab=0;pyx; pygen_name; latex_name; capt_args = [] }


let count_tab s = 
let rec aux n = match s.[n] with
|'\t' -> 1+ aux (n-1) 
| _ -> 0 in
(String.length s) -1  |> aux 

let print_tabs env= 
	for i=1 to env.ntab do
		 output_char env.pyx '\t' 
	done

let latex env s= print_tabs env; fprintf env.pyx "__latex__(r'''%s''')\n" s; env
let pyinclusion env s = print_tabs env; fprintf env.pyx "__pyinclusion__(%s)\n" s; env

let code env s = String.trim s |> fprintf env.pyx "%s \n"; env

let write_text env = function
	| `Raw, s -> latex env s
	| `Inclusion, s -> pyinclusion env s

let write_code env = function
	| `Raw, s -> let env' = { env with ntab = count_tab s} in code env' s
	| `Inclusion, s -> pyinclusion env s

let escaped env s = 
	for k=0 to String.length s-1 do 
		match s.[k] with
			| '{' -> output_string  env.pyx "{{"
			| '}' -> output_string  env.pyx "}}"
			|  c  -> output_char env.pyx c
	done; env

let write_capture env = function 
	| `Raw, s -> escaped env s
	| `Inclusion, s -> fprintf env.pyx "{}";  { env with capt_args = s::env.capt_args }

let node_code env cs k = k env cs

let node_capture env cpts k = 
let ()  = print_tabs env; fprintf env.pyx "__latex__(r'''" in
let env = k env cpts in
let () = fprintf env.pyx "'''.format(%s) )\n" (String.concat "," @@  List.rev env.capt_args) in
	env
  

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


(* 
	write_text=( fun env kind s -> 
and
	
	match kind with
		| Text | Capture -> print_tabs env ;  "__latex__" $ !s 
		| Code -> env.ntab<- count_tab s ; String.trim s |> fprintf env.pyx "%s \n"
		| Inclusion -> begin match String.trim s with
				| "" -> ()
				| s -> print_tabs env ; "__pynclusion__" $ s	
			end 
		);
	write_node =( fun env kind hydres k ->
		let ($) s a  = fprintf env.pyx "%s(%s)\n" s a in
		 match kind with
		| Capture ->  print_tabs env ;  "__newContext__" $ "";
				k env hydres ;
				print_tabs env; "__popContext__" $ ""
		| _         ->  k env hydres
	);
	end_compilation;
}*)
