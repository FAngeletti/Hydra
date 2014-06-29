open Lexer
open Printf

type kind = Tex | Python | Inclusion
type hydre = Text of string | Node of kind*hydres 
and hydres = hydre list

let sprint_kind = function Tex ->  "Tex" | Python -> "Python" | Inclusion -> "Inclusion" 


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
		| _ -> error loc_token "closing an unopened block" 
in 
continue parse_tex 



let rec sprint hydres = String.concat ";" ( List.map sprintEl hydres ) 
and sprintEl= function
| Text(s) -> sprintf "Text<<%s>>"  s 
| Node(kind, hs) -> sprintf "Node{%s}<<%s>>" (sprint_kind kind)  (sprint hs) 


