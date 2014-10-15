open Lexer
open Printf
type z = Nil_Z
type 'a fragment = 'a * string
type simple  = [`Raw| `Inclusion ] fragment  
type _ extension = 
	| S : simple -> z extension
	| A : z extension -> 'a extension extension  
	| E : 'a extension list -> ('a extension) extension

type basic = z extension
type with_capture = basic extension

type hydre = with_capture extension
and hydres = hydre list



exception Hydra_syntax_error of string 

let str_loc loc =  Printf.sprintf "(col %d, line %d)" loc.ncol loc.nline
let sp = Printf.sprintf

let error {start;ende;token} messg= 
	let messgE= Printf.sprintf 
	"Syntax error : %s while reading token %s from %s to %s" messg (str_token token)  (str_loc start) (str_loc ende) 
	in
	raise @@ Hydra_syntax_error messgE

let verify error extern inner = match extern, inner with
	| Inclusion, Code -> error"[Code] environment cannot be nested inside [Inclusion] environment" 	
	| _ -> () (* fine *) 

let close_error loc_token k = error loc_token @@ sp "closing an unopened [%s] environment" (str_kind k)   	 
let unexpected_end loc_token k = error loc_token @@ sp "unexped end inside [%s] environment" (str_kind k)  
let no_nesting loc_token =  error loc_token "no other environment are allowed inside [Inclusion] environment" 
let wrong_polarity loc_token p expected =  error loc_token @@ sp "wrong polarity %s when expecting %s" ( str_polarity p ) (str_polarity expected)  

let parse_hydra token_source  =
	let continue f= f @@ token_source () in
	let cont_p p f = continue (f @@ reverse p) in
	let ( ||> ) a b = a::(continue b) in
	let rec parse_text loc_token = match loc_token.token with 
		| Raw(a) -> A(S(`Raw,a)) ||> parse_text
		| Keyword(k,R)  -> close_error loc_token k
		| Keyword(Inclusion, p ) -> A(cont_p p parse_inclusion) ||> parse_text
		| Keyword(Code, p ) ->  E(cont_p p parse_code) ||> parse_text
		| Keyword(Capture, _) -> error loc_token "capture environment are not authorised inside text"
		| End -> []
	and parse_code p loc_token = match loc_token.token with
		| Raw(a) -> A(S(`Raw,a)) ||> parse_code p
		| Keyword(Code, polarity) when polarity = p -> [] 
		| Keyword(k,R) -> close_error loc_token k
		| Keyword(Code, polarity) -> wrong_polarity loc_token polarity p
		| Keyword(Inclusion,p) -> A(cont_p p parse_inclusion) ||> parse_code p
		| Keyword(Capture,polarity) -> E(cont_p polarity parse_capture) ||> parse_code p
		| End -> unexpected_end loc_token Code
	and parse_capture p loc_token = match loc_token.token with
		| Raw(a) -> S(`Raw, a) ||> parse_capture p
		| Keyword(Capture, polarity) when polarity = p -> [] 
		| Keyword(k,R) -> close_error loc_token k
		| Keyword(Capture, polarity) -> wrong_polarity loc_token polarity p
		| Keyword(Inclusion,polarity) -> (cont_p polarity parse_inclusion) ||> parse_capture p
		| Keyword(Code,p ) -> error loc_token "code environment are not authorised inside capture environment"
		| End -> unexpected_end loc_token Capture
	and parse_inclusion p loc_token = match loc_token.token with
		| Raw(a) -> continue (check_inclusion p) ;  S(`Inclusion,a)
		| Keyword( k , p ) -> no_nesting loc_token
		| End -> unexpected_end loc_token Inclusion
	and check_inclusion p loc_token = match loc_token.token with
		| Keyword(Inclusion, polarity) when p==polarity -> () 
		| Keyword(_,_) | Raw _ -> no_nesting loc_token
		| End -> unexpected_end loc_token Inclusion 	
	in 
	continue parse_text 



let rec sprint: type a. a extension list -> string = fun hydres ->  String.concat ";" ( List.map sprintEl hydres ) 
and sprintEl: type a. a extension -> string = function
| S (`Raw,s) -> sp "Raw[%s]" s
| S (`Inclusion, s) -> sp "Inclusion[%s]" s
| A h -> sprintEl h
| E hs -> sp "Extension[\n %s \n]" (sprint hs)
