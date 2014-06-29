open Printf
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


