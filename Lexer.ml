open Printf

type polarity = L | R | T
let reverse = function L -> R | R->L | T -> T 

type kind = Code | Inclusion | Capture 

let str_kind = function Code ->  "Code" | Inclusion -> "Inclusion" | Capture -> "Capture" 

type raw_keyword = int * kind * polarity   
type automaton  ={state: raw_keyword option ; transition:char -> automaton }

(* 

Python:
Code §: :§
Inclusion <§ §>
Sub-context ¤ ¤

Generic:
Code {@ @}
Inclusion ⟦x⟧
Capture block @[ ]@

*) 


let py = "\194\167"
let op_capt="\226\159\166"
let cl_capt="\226\159\167"

let final len kind polarity transition = {state = Some (len,kind,polarity); transition}
let no_key transition = { state = None ; transition } 

let rec gen_a = {state=None; transition = g_start}
and key len k p = final len k p g_start  
and g_start = function
	| '{' -> no_key g_code_l1
        | '@' -> no_key g_at1
        | ']' -> no_key g_capt_r1
	| '\226' -> no_key g_inc1
	| _ -> gen_a 
and g_code_l1 = function
	| '@' -> key 2 Code L
	| c -> g_start c
and g_at1 = function
	| '}' -> key 2 Code R
	| '[' -> key 2 Capture L
	| c -> g_start c
and g_capt_r1 = function
	| '@' -> key  2 Capture R
	| c -> g_start c
and g_inc1 = function
	| '\159' -> no_key g_inc2
	| c -> g_start c
and g_inc2 = function
	| '\166' -> key 3 Inclusion  L 
	| '\167' -> key 3 Inclusion  R
	| c -> g_start c 



let rec py_aut = {state=None; transition=py_start }
and key len k p = final len k p py_start
and py_start = function
	  '\194' -> no_key py_op
	| ':' -> no_key py_close
	| '<' -> no_key py_ncl
        | _ -> py_aut;  
and py_op = function 
	| '\167' -> no_key py_nclose
	| '\164' -> key 2 Capture T  
 	| c ->  py_start c
and py_close = function
	| '\194' -> no_key py_close_2
 	| c ->  py_start c
and py_close_2 = function
	| '\167' -> key 3 Code R
 	| c ->  py_start c
and py_ncl = function
	| '\194' -> no_key py_ncl_2 
	| c ->  py_start c
and py_ncl_2 = function
	| '\167' ->  key 3  Inclusion L
 	| c ->  py_start c
and py_nclose = function
	| '>' ->  key 3 Inclusion R
	| ':' ->  key 3 Code L
 	| c ->  py_start c



let may f x= function
| None -> x
| Some y -> f x y

type loc = { nchar : int ; ncol : int ; nline:int}
type token = Raw of string | Keyword of kind*polarity | End
type localized_token = {start : loc; ende : loc ; token : token }
  
exception End_of_text

(* Assume that no keyword is a prefix of any other keywords *)
let lex automaton st= 
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
	let commit_with_keyword start ende  (len, k, p ) =
		let mid=decr ende len in
		glob_loc := incr ende;
		token_stack := {start=mid;ende; token=Keyword (k,p)} ::!token_stack;
		raw start mid in 
	let rec lexing start aut stack ende=
		let c =st.[ende.nchar] in
		let aut = aut.transition c in
			match (aut.state, ende.nchar + 1 < lmax ) with
			| (Some k, _ ) -> commit_with_keyword start ende k
			| (None,true) ->  lexing start aut stack (incr_char ende c)
			| (None,false) ->   commit start ende
	in
	fun () -> match (!token_stack, !glob_loc.nchar<lmax) with
		| a :: q, _ -> token_stack:= q ; a
		| [], true ->  ( lexing !glob_loc automaton [] !glob_loc )
		| [], false -> { start= !glob_loc; ende= !glob_loc; token=End} 
 



let iter f lex= 
	let rec loop () =
		match lex () with 
		| End -> ()
		| x -> f x ; loop () 
	in 
loop ()

let str_polarity = function
	| L -> "L"
	| R -> "R" 
	| T -> "T"


let str_token = function Raw s -> s | Keyword (k,p)  ->sprintf "Keyword{%s|%s}\n" (str_kind k) (str_polarity p)  | End -> "end"

let print_lex = function
| {token=Raw s; _} -> printf "Text{%s}\n" s
| {token=Keyword (k,p) ; _} -> printf "Keyword{%s|%s}\n" (str_kind k) (str_polarity p) 
| {token=End; _ } -> ()


