open Lexer
open Parser
exception Illformed_ast

type 'env backend = {
	init_env : string -> 'env;
	preambule : 'env -> 'env;
	write_text : 'env -> simple -> 'env;
	write_code : 'env -> simple -> 'env;
	write_capture : 'env -> simple -> 'env;
	node_code: 'env -> with_capture list -> ('env->with_capture list -> 'env) -> 'env;
	node_capture : 'env -> basic list -> ('env -> basic list -> 'env) -> 'env;  
	end_compilation : 'env -> unit
}


let transcompile backend basename hydres=
	let init_env = backend.init_env basename in
	let rec transcompile_text env hydres = List.fold_left  transcompile_text_node env hydres 
	and transcompile_text_node env = function
		| A ( S f ) -> backend.write_text env f
		| E cs -> backend.node_code env cs transcompile_code 
	and transcompile_code env cs = List.fold_left transcompile_code_node env cs 
	and transcompile_code_node env = function
		| A ( S f ) -> backend.write_code env f
		| E cpt -> backend.node_capture env cpt transcompile_capture
	and transcompile_capture env cpt = List.fold_left transcompile_node_capture env cpt
	and transcompile_node_capture env = function
		| S f -> backend.write_capture env f
	in 
	let env = init_env |> backend.preambule in
	transcompile_text env hydres |> backend.end_compilation

