
type hydre = Text of string | Python of  hydres | PyInclusion of hydres |  TexInclusion of hydres 
and hydres = hydre list


type automaton  ={state: string option ; transition:char -> automaton }

let py="\194\167"
let incl="\194\164"

let rec aut_start_t = {state=None; transition=aut_start }
and aut_start = function
	  '\194' ->   {state=None; transition = aut_op}
	| ':' -> {state=None; transition = aut_pyclose}
	| '<' -> {state=None; transition = aut_pyncl}
        | _ -> aut_start_t ;  
and aut_op = function 
	| '\167' -> {state=None; transition=aut_pynclose}
	| '\164' -> {state=Some "¤"; transition=aut_start} 
 	| _ ->  aut_start_t
and aut_pyclose = function
	| '\194' -> {state=None; transition=aut_pyclose_2}
 	| _ ->  aut_start_t
and aut_pyclose_2 = function
	| '\167' -> { state=Some ":§"; transition=aut_start}
 	| _ ->  aut_start_t
and aut_pyncl = function
	| '\194' ->  { state= None; transition=aut_pyncl_2}
 	| _ ->  aut_start_t
and aut_pyncl_2 = function
	| '\167' ->  { state= Some "<§" ; transition=aut_start }
 	| _ ->  aut_start_t
and aut_pynclose = function
	| '>' ->  { state= Some "§>" ; transition=aut_start}
	| ':' ->  { state= Some "§:" ; transition=aut_start}
 	| _ ->  aut_start_t

let may f x= function
| None -> x
| Some y -> f x y

type token = Raw of string | Keyword of string | End
exception End_of_text

let lex st= 
	let ( -- ) start pos = String.sub st start (pos-start+1) in
	let token_stack =ref [] in
	let glob_pos= ref 0 in
	let lmax = String.length st in
	let commit start pos= glob_pos := pos+1; Raw (start -- pos ) in
	let commit_with_keyword start (end_pos , symb) =
		let spos=end_pos -String.length symb in
		glob_pos := end_pos+1;
		token_stack := Keyword symb ::!token_stack;
		Raw ( start -- spos ) in 
	let rec lexing start aut stack pos=
		let c =st.[pos] in
		let aut = aut.transition c in
		let stack  = may (fun stack newf -> (pos,newf)::stack) stack aut.state in
		begin
			match (aut==aut_start_t, stack, pos + 1 < lmax ) with
			| (true, a::q, _ ) -> commit_with_keyword start a
			| (_,_,true) ->  lexing start aut stack (pos+1)
			| (_,_,false) ->   commit start pos
		end in
	fun () -> match (!token_stack, !glob_pos<lmax) with
		| a :: q, _ -> token_stack:= q ; a
		| [], true ->  ( lexing !glob_pos aut_start_t [] !glob_pos )
		| [], false -> End 
 



let iter f lex= 
	let rec loop () =
		match lex () with 
		| End -> ()
		| x -> f x ; loop () 
	in 
loop ()

open Printf 

let print_lex = function
| Raw s -> printf "Text{%s}\n" s
| Keyword s -> printf "Keyword{%s}\n" s 
| End -> ()



exception Hydra_syntax_error of string 

let parse_hydra token_source  =
	let continue f= f @@ token_source () in
	let ( ||> ) a b = a::(continue b) in
	let rec parse_tex= function
		| Raw(a) -> Text(a) ||> parse_tex
		| Keyword "<§" -> PyInclusion (continue parse_pynclusion) ||> parse_tex
		| Keyword "§:" -> Python (continue parse_python) ||> parse_tex
		| End -> []
		| Keyword "¤" -> raise @@ Hydra_syntax_error "Tex inclusion are not allowed in Tex mode"
		| _ -> raise @@ Hydra_syntax_error "Closing an unopened block"
	and parse_pynclusion = function
			| Raw(a) ->  Text(a)::(continue parse_pynclusion)
			| Keyword "§>" -> []
			| Keyword "¤" -> TexInclusion(continue parse_texinclusion) ||> parse_pynclusion
			| Keyword "§:" -> raise @@ Hydra_syntax_error "Python mode is not allowed inside python inclusion"
			| _ -> raise @@ Hydra_syntax_error "Closing an unopened block"
	and parse_python = function
		| Keyword ":§" -> []
		| Raw(p) -> Text(p) ||> parse_python
		| Keyword "<§" ->  PyInclusion(continue parse_pynclusion) ||> parse_python
		| Keyword "¤" ->  TexInclusion(continue parse_texinclusion) ||> parse_python
		| Keyword "§:" -> raise @@ Hydra_syntax_error "Already in python mode"
		| _ -> raise @@ Hydra_syntax_error "Closing an unopened block"
	and parse_texinclusion = function
		| Keyword "¤"-> []
		| Raw t -> Text(t) ||> parse_texinclusion
		| Keyword "<§" ->  PyInclusion(continue parse_pynclusion) ||> parse_texinclusion
		| Keyword "§:" -> raise @@ Hydra_syntax_error "Python mode not allowed in tex inclusion"
		| _ -> raise @@ Hydra_syntax_error "Closing an unopened block" 
in 
continue parse_tex 



let rec sprint hydres = String.concat ";" ( List.map sprintEl hydres ) 
and sprintEl= function
| Text(s) -> sprintf "Text<<%s>>"  s 
| Python(hs) -> sprintf "Python<<%s>>" (sprint hs) 
| PyInclusion(hs) -> sprintf "Inclusion[Python]<<%s>>" (sprint hs)
| TexInclusion(hs) -> sprintf "Inclusion[Latex]<<%s>>" (sprint hs)



let preambule chan latexname =
fprintf chan 
"#!/usr/bin/env python 
#This is an automatically generated python generator file.
__latexFile__=open('%s', 'w+')

def __writeLatexFile__(str):
	__latexFile__.write(str)

def __readLatexFile__():
	return str(__latexFile__)

__ContextStack__ = [ (__readLatexFile__,__writeLatexFile__) ]

def __newContext__():
	context=\"\"
	def write(str):
		context+=str
	def read():
		return context 
	__ContextStack__.append( (read,write) )


def __latex__(str):
	__ContextStack__[0][1](str)

def __pynclusion__(obj):
	try:
		s=obj.latex()
	except AttributeError:
		s=str(obj)
	__latex__(s)

def __popContext__():
	(r,w)=__ContextStack__.pop()
	__latex__(r())


" latexname


let pygenName s =   "gen_"^s^".py" 
let latexName s =   s^".tex" 


type env = {mutable ntab : int; pyx : out_channel }

let init_env basename = 
let pyx = pygenName basename |> open_out in
{ntab=0;pyx}


let count_tab s = 
let rec aux n = match s.[n] with
|'\t' -> 1+ aux (n-1) 
| _ -> 0 in
(String.length s) -1  |> aux 

let print_tabs env= 
	for i=1 to env.ntab do
		 output_char env.pyx '\t' 
	done

let latexW env  =print_tabs env ;  fprintf env.pyx "__latex__(r'''%s''')\n"  
let pythonW env s = String.trim s |> fprintf env.pyx "%s \n"
let inpythonW env s = 
	print_tabs env ;
	match String.trim s with
	| "" -> ()
	| s -> fprintf env.pyx "__pynclusion__(%s)\n" s
let intexW env middle = 
	fprintf env.pyx "__newContext__()\n";
	middle () ;
	print_tabs env; fprintf env.pyx "__popContext__()\n"

exception Illformed_ast

let transcompile mode env hydres  = List.iter (mode env) hydres

let rec latex_mode env = function
| Text(s) -> latexW env s
| Python(s) -> transcompile (python_mode) env s
| PyInclusion(s) -> transcompile (python_incl_mode) env s
| _ -> raise Illformed_ast
and python_mode env = function
| Text(s) -> env.ntab<- count_tab s; pythonW env s
| PyInclusion(s) -> transcompile (python_incl_mode) env s
| TexInclusion(s) -> intexW env @@  fun () -> transcompile (tex_incl_mode) env s 
| _ -> raise Illformed_ast
and python_incl_mode env = function
| Text(s) -> inpythonW env s
| TexInclusion(s) -> intexW env @@ fun () -> transcompile (tex_incl_mode) env s 
| _ -> raise Illformed_ast
and tex_incl_mode env = function
| Text(s) -> latexW env s
| PyInclusion(s) ->  transcompile (python_incl_mode) env s  
| _ -> raise Illformed_ast


(*

let rec  transf mode env hydres  = List.iter (mode env) hydres 
and latex env = function
| Text(s) ->  latexW env.pyx s
| Section(name, hydres) ->   transf  latex (select env name) hydres  
| Python(cont, hydres) ->  transf python (select env cont)  hydres
| PyInclusion(s) -> inpythonW 0 env.pyx s 
and python env = function
| Text(s) ->  env.ntab<- count_tab s;  pythonW ( "" |: env ) s
| Section(name,hydres) -> transf latex (select env name) hydres 
| Python(cont, hydres) ->  transf python (select env cont)  hydres
| PyInclusion(s) -> inpythonW env.ntab env.pyx s 
*)


let load f= 
 let cin = open_in f in
 let n= in_channel_length cin in
 let s=String.create n in
 ignore(input cin s 0 n); s   


let ()=  		
	let path=Sys.argv.(1) in
	let basename = Filename.chop_extension path in
	let gen = pygenName basename in 
	let env = init_env basename in
	latexName basename |> preambule env.pyx; 
	load path |> lex |> parse_hydra |> transcompile latex_mode env; close_out env.pyx; 
	let error1 = Sys.command ("chmod u+x "^gen) in
	let error2 = Sys.command ("./"^gen) in
	match (error1,error2)  with
		| 0,0 -> ()
		| _ -> printf "Sorry something has gone wrong \n"
	 






  
