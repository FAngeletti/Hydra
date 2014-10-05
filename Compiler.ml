open Lexer
open Parser
exception Illformed_ast

type 'env backend = {
	init_env : string -> 'env;
	preambule : 'env -> unit;
	write_node : 'env -> kind -> hydres -> ('env -> hydres -> unit ) -> unit;
	write_text : 'env -> kind -> string -> unit;
	end_compilation : 'env -> unit
}


let transcompile backend basename hydres=
	let init_env = backend.init_env basename in
	let rec transcompile current_kind env hydres = List.iter (transcompile_node current_kind env) hydres
	and transcompile_node current_kind env = function
		| Text(s) -> backend.write_text env current_kind s
		| Node( new_kind, hydres ) -> backend.write_node env new_kind hydres (transcompile new_kind) in
	backend.preambule init_env;
	transcompile Text init_env hydres;
	backend.end_compilation init_env

