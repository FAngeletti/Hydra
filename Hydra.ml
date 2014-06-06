
open Printf 

type kind = Tex | Python | Inclusion
type hydre = Text of string | Node of kind*hydres 
and hydres = hydre list

let sprint_kind = function Tex ->  "Tex" | Python -> "Python" | Inclusion -> "Inclusion" 

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

type loc = { nchar : int ; ncol : int ; nline:int}
type token = Raw of string | Keyword of string | End
type localized_token = {start : loc; ende : loc ; token : token }
  
exception End_of_text

let lex st= 
	let ( -- ) start pos = String.sub st (start.nchar) (pos.nchar-start.nchar+1) in
	let token_stack =ref [] in
	let glob_loc = ref {nchar=0; ncol=1; nline=1} in
	let incr loc= {loc with nchar = loc.nchar+1; ncol=loc.ncol+1} in 
	let incr_char loc c= 
		let loc'= incr loc in	
		match c with
			| '\n' ->  {loc' with nline=loc.nline+1; ncol=1 } 
			| _ -> loc'  
	and decr loc k = {loc with nchar = loc.nchar-k; ncol=loc.ncol-k} in
	let raw start ende = {start; ende ; token=Raw( start --ende) } in
	let lmax = String.length st in
	let commit start ende= 
		glob_loc :=  incr ende;
		raw start ende in
	let commit_with_keyword start (ende , symb) =
		let len= String.length symb in
		let mid=decr ende len in
		glob_loc := incr ende;
		token_stack := {start=mid;ende; token=Keyword symb} ::!token_stack;
		raw start mid in 
	let rec lexing start aut stack ende=
		let c =st.[ende.nchar] in
		let aut = aut.transition c in
		let stack  = may (fun stack newf -> (ende,newf)::stack) stack aut.state in
		begin
			match (aut==aut_start_t, stack, ende.nchar + 1 < lmax ) with
			| (true, a::q, _ ) -> commit_with_keyword start a
			| (_,_,true) ->  lexing start aut stack (incr_char ende c)
			| (_,_,false) ->   commit start ende
		end in
	fun () -> match (!token_stack, !glob_loc.nchar<lmax) with
		| a :: q, _ -> token_stack:= q ; a
		| [], true ->  ( lexing !glob_loc aut_start_t [] !glob_loc )
		| [], false -> { start= !glob_loc; ende= !glob_loc; token=End} 
 



let iter f lex= 
	let rec loop () =
		match lex () with 
		| End -> ()
		| x -> f x ; loop () 
	in 
loop ()


let print_lex = function
| {token=Raw s; _} -> printf "Text{%s}\n" s
| {token=Keyword s; _} -> printf "Keyword{%s}\n" s 
| {token=End; _ } -> ()



exception Hydra_syntax_error of string 

let error {start;ende;token} messg= 
	let sprint_loc loc = Printf.sprintf "(col %d, line %d)" loc.ncol loc.nline in
	let sprint_token = function Raw s -> s | Keyword s -> s | End -> "end"  in
	let messgE= Printf.sprintf 
	"Syntax error : %s while reading token %s from %s to %s" messg (sprint_token token)  (sprint_loc start) (sprint_loc ende) 
	in
	raise @@ Hydra_syntax_error messgE
 
let parse_hydra token_source  =
	let continue f= f @@ token_source () in
	let ( ||> ) a b = a::(continue b) in
	let rec parse_tex loc_token= match loc_token.token with 
		| Raw(a) -> Text(a) ||> parse_tex
		| Keyword "<§" -> Node (Inclusion, continue parse_pynclusion) ||> parse_tex
		| Keyword "§:" -> Node(Python, continue parse_python) ||> parse_tex
		| End -> []
		| Keyword "¤" -> error loc_token "latex inclusion are not allowed in latex mode"
		| _ -> error loc_token "closing an unopened block"
	and parse_pynclusion loc_token= match loc_token.token with 
			| Raw(a) ->  Text(a)::(continue parse_pynclusion)
			| Keyword "§>" -> []
			| Keyword "¤" -> Node(Tex, continue parse_texinclusion) ||> parse_pynclusion
			| Keyword "§:" -> error loc_token "python mode is not allowed inside python inclusion"
			| _ -> error loc_token "closing an unopened block"
	and parse_python loc_token= match loc_token.token with 
		| Keyword ":§" -> []
		| Raw(p) -> Text(p) ||> parse_python
		| Keyword "<§" ->  Node(Inclusion,continue parse_pynclusion) ||> parse_python
		| Keyword "¤" ->  Node(Tex, continue parse_texinclusion) ||> parse_python
		| Keyword "§:" -> error loc_token "already in python mode"
		| _ -> error loc_token "closing an unopened block"
	and parse_texinclusion loc_token= match loc_token.token with 
		| Keyword "¤"-> []
		| Raw t -> Text(t) ||> parse_texinclusion
		| Keyword "<§" ->  Node(Inclusion, continue parse_pynclusion) ||> parse_texinclusion
		| Keyword "§:" -> error loc_token "python mode not allowed in tex inclusion"
		| _ -> error loc_token "Closing an unopened block" 
in 
continue parse_tex 



let rec sprint hydres = String.concat ";" ( List.map sprintEl hydres ) 
and sprintEl= function
| Text(s) -> sprintf "Text<<%s>>"  s 
| Node(kind, hs) -> sprintf "Node{%s}<<%s>>" (sprint_kind kind)  (sprint hs) 




type env = {mutable ntab : int; pyx : out_channel ; latex_name : string; pygen_name : string  }

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

exception Illformed_ast

type 'env backend = {
	init_env : string -> 'env;
	preambule : 'env -> unit;
	write_node : 'env -> kind -> kind -> (unit -> unit ) -> unit;
	write_text : 'env -> kind -> string -> unit;
	end_compilation : 'env -> unit
}

let latex_backend = {
	init_env;
	preambule; 
	write_text=( fun env kind s -> match kind with
		| Tex -> print_tabs env ;  fprintf env.pyx "__latex__(r'''%s''')\n"   s
		| Python -> env.ntab<- count_tab s ; String.trim s |> fprintf env.pyx "%s \n"
		| Inclusion -> begin match String.trim s with
				| "" -> ()
				| s -> print_tabs env ; fprintf env.pyx "__pynclusion__(%s)\n" s	
			end
		);
	write_node =( fun env ext_kind inner_kind delayed -> match (ext_kind,inner_kind) with
		| (_, Tex ) ->  print_tabs env ; fprintf env.pyx "__newContext__()\n";
				delayed () ;
				print_tabs env; fprintf env.pyx "__popContext__()\n"
		| _         ->  delayed ()
	);
	end_compilation;
}

type py_env = {py : out_channel; pyname : string }

let python_backend = {
	init_env  = (fun basename ->  let pyname = basename^".py" in {pyname; py = open_out pyname} ) ; 
	preambule =( fun env -> output_string env.py "r'''" ); 
	write_text=( fun env kind s -> output_string env.py s);
	write_node =( fun env ext_kind inner_kind delayed -> match (ext_kind,inner_kind) with
		| (_, Tex )   ->  fprintf env.py " r''' "; delayed () ; fprintf env.py " ''' "
		| (Tex,Python) -> fprintf env.py " ''' \n "; delayed () ;  fprintf env.py " \n r''' "
		| _         ->     delayed ()
	);
	end_compilation  = (fun  env -> output_string env.py "'''"; close_out env.py);
}




let transcompile backend basename hydres=
	let env = backend.init_env basename in
	let rec transcompile current_kind hydres = List.iter (transcompile_node current_kind) hydres
	and transcompile_node current_kind = function
		| Text(s) -> backend.write_text env current_kind s
		| Node( new_kind, hydres ) -> backend.write_node env current_kind new_kind @@ fun () -> transcompile new_kind hydres in
	backend.preambule env;
	transcompile Tex hydres;
	backend.end_compilation env

let load f= 
 let cin = open_in f in
 let n= in_channel_length cin in
 let s=Bytes.create n in
 ignore(input cin s 0 n); Bytes.to_string s   

exception Unknown_mode
let ()=  
	let rec mode = ref "latex" in		
	let spec = ["--mode", Arg.Set_string mode, "Define the output mode"] in
	let rpath = ref "" in
	let () = Arg.parse spec (fun path -> rpath:=path) "hydra --mode `mode source.hyd" in
	let path= !rpath in
	let basename = Filename.chop_extension path in
	let compile = match !mode with 
		| "latex" -> transcompile latex_backend 
		| "python" -> transcompile python_backend
		| _ -> raise Unknown_mode  in 
	let ast =  load path |> lex |> parse_hydra in
	compile basename ast 
 






  
