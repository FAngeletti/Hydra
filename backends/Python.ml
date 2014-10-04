open Lexer
open Compiler
open Printf
open Parser
type py_env = {py : out_channel; pyname : string }

let backend = transcompile {
	init_env  = (fun basename ->  let pyname = basename^".py" in {pyname; py = open_out pyname} ) ; 
	preambule =( fun env -> output_string env.py "r'''" ); 
	write_text=( fun env kind s -> output_string env.py s);
	write_node =( fun env kind hydres k -> match kind with
		| Capture   ->  fprintf env.py " r''' "; k hydres ; fprintf env.py " ''' "
		| Code -> fprintf env.py " ''' \n "; k hydres ;  fprintf env.py " \n r''' "
		| _         ->     k hydres
	);
	end_compilation  = (fun  env -> output_string env.py "'''"; close_out env.py);
}


