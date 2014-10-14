open Lexer
open Compiler
open Printf
open Parser
type py_env = {py : out_channel; pyname : string }

let write env s= output_string env.py s; env 
let write_f env = function
|_ , s -> write env s

let backend = transcompile {
	init_env  = (fun basename ->  let pyname = basename^".py" in {pyname; py = open_out pyname} ) ; 
	preambule =( fun env -> write env "r'''" ); 
	write_text= write_f;
	write_code= write_f;
	write_capture = write_f;
	node_code = ( fun env hydres k -> write env " r''' ";  k env hydres; write env " ''' "; env );
	node_capture = (fun env hydres k -> write env " r''' "; k env hydres ; write env " ''' "; env );
	end_compilation  = (fun  env -> output_string env.py "'''"; close_out env.py);
}


