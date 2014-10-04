open Lexer
open Printf


type hydre =  Text of string | Node of kind*hydres 
and hydres = hydre list



exception Hydra_syntax_error of string 

let str_loc loc =  Printf.sprintf "(col %d, line %d)" loc.ncol loc.nline

let error {start;ende;token} messg= 
	let messgE= Printf.sprintf 
	"Syntax error : %s while reading token %s from %s to %s" messg (str_token token)  (str_loc start) (str_loc ende) 
	in
	raise @@ Hydra_syntax_error messgE

let verify error extern inner = match extern, inner with
	| Inclusion, Code -> error"[Code] environment cannot be nested inside [Inclusion] environment" 	
	| _ -> () (* fine *) 
 
let parse_hydra token_source  =
	let sp = Printf.sprintf in
	let continue f= f @@ token_source () in
	let ( ||> ) a b = a::(continue b) in
	let rec parse_text loc_token= match loc_token.token with 
		| Raw(a) -> Text(a) ||> parse_text
		| Keyword(Capture, _) -> error loc_token "capture environment are not authorised inside text"
		| Keyword(k,R) -> error loc_token @@ sp "closing an unopened [%s] environment" (str_kind k)   		
		| Keyword (k, p) -> Node(k, continue @@ parse_kind (reverse p) k ) ||> parse_text
		| End -> []
	and parse_kind polarity kind loc_token = match loc_token.token with
		| Raw(a) -> Text(a) ||> parse_kind polarity kind
		| Keyword( k , p ) -> begin match (k=kind , p=polarity, p) with
			| true, true, _ -> []
			| true, false, _ -> error loc_token @@ sp "[%s] environment nesting" (str_kind kind)
			| false, _ , R -> error loc_token @@ sp "closing an unopened [%s] environment" (str_kind kind)   
			| false, _, p -> verify (error loc_token) kind k; Node(k,continue @@ parse_kind (reverse p) k ) ||> parse_kind polarity kind  
		end
		| End -> raise @@ Hydra_syntax_error ( sp "Unexpected end of file when in [%s] environment " (str_kind kind) )
	
	in 
	continue parse_text 



let rec sprint hydres = String.concat ";" ( List.map sprintEl hydres ) 
and sprintEl= function
| Text(s) -> sprintf "Text<<%s>>"  s 
| Node(kind, hs) -> sprintf "Node{%s}<<%s>>" (str_kind kind)  (sprint hs) 


