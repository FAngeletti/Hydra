open Compiler
open Parser
open Lexer

(* TODO : Working nested environment inside code environment *)

type interpreter = { out : out_channel; inp : in_channel }


type env = { target :out_channel ;  ocaml: interpreter    }

let write env s= output_string env.target s
 

let init_env basename = 
	let target  = basename |> open_out 
	and inp, out = Unix.open_process "ocaml" in
	let () = ignore [input_line inp; input_line inp] in
	{ target ; ocaml = {out;inp} }

let preambule env = ()

let discard {inp;_} = while input_line inp <> "# Exception: End_of_file." do () done

let search char s start  =
	let rec search start = 
	match start= String.length s with
		| true -> raise Not_found
		| false -> if s.[start] = char then start else search @@ start+1 in
	search start 


exception Type_error of string
exception Not_implemented of string

let send ocaml s = 
	output_string ocaml.out s ; flush ocaml.out

let parse_toplevel ocaml =
	let s = input_line ocaml.inp in
	let type_start= search ':' s 0 in
	let type_stop = search '=' s (type_start) in
	let type_s = String.trim @@ String.sub s (type_start+1) (type_stop-type_start-1) in
	match type_s with
		| "string" | "bytes" -> begin    
		let start = search '"' s type_stop in
		let stop = search  '"' s (start+1) in
		String.sub s (start+1) (stop-start-1) 
		end
		| s -> raise @@ Type_error type_s

let eval ocaml s = 
	send ocaml (s^";;\n"); 
	parse_toplevel ocaml

let write_text env kind s = 
	match kind with
	| Text | Capture -> write env s 
	| Code -> List.iter (send env.ocaml) [s; ";;\n"; "let _ = raise End_of_file;;\n"] ;  discard env.ocaml 
	| Inclusion -> let m = eval env.ocaml s in write env m 
			
let write_node env kind hydres k = match kind with
	| Capture ->  raise @@ Not_implemented "Capture in interpreted mode" 
	| _ -> k env hydres
			 
let end_compilation {ocaml;_} = ignore ( Unix.close_process (ocaml.inp,ocaml.out) )
  

let backend = transcompile {
	init_env; 
	preambule;
	write_node;
	write_text;
	end_compilation}

