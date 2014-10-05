open Printf
open Lexer
open Compiler
open Parser
type env = { pyx : out_channel ; mutable ntab : int ; pygen_name : string ; latex_name: string }
let preambule env =
fprintf env.pyx 
"#!/usr/bin/env python 
#This is an automatically generated python generator file.
__latexFile__=open('%s', 'w+')

def __writeLatexFile__(str):
	__latexFile__.write(str)


__ContextStack__ = []

def __newContext__():
	__ContextStack__.append(\"\")


def __latex__(str):
	if len(__ContextStack__) > 0 :
		__ContextStack__[0]+=(str)
	else:
		__writeLatexFile__(str)

def __pynclusion__(obj):
	try:
		s=obj.latex()
	except AttributeError:
		s=str(obj)
	__latex__(s)

def __popContext__():
	s=__ContextStack__.pop()
	__latex__(s)


" env.latex_name

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
	{ntab=0;pyx; pygen_name; latex_name }


let count_tab s = 
let rec aux n = match s.[n] with
|'\t' -> 1+ aux (n-1) 
| _ -> 0 in
(String.length s) -1  |> aux 

let print_tabs env= 
	for i=1 to env.ntab do
		 output_char env.pyx '\t' 
	done

let backend = transcompile {
	init_env;
	preambule; 
	write_text=( fun env kind s -> 
	let ($) s a  = fprintf env.pyx "%s(%s)\n" s a and
	(!) s = sprintf "r'''%s'''" s  in
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
}
