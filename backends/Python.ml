open Compiler
open Printf
open Parser
type py_env = {py : out_channel; pyname : string }

let backend = transcompile {
	init_env  = (fun basename ->  let pyname = basename^".py" in {pyname; py = open_out pyname} ) ; 
	preambule =( fun env -> output_string env.py "r'''" ); 
	write_text=( fun env kind s -> output_string env.py s);
	write_node =( fun env ext_kind inner_kind delayed -> match (ext_kind,inner_kind) with
		| (_, Tex )   ->  fprintf env.py " r''' "; delayed () ; fprintf env.py " ''' "
		| (Tex,Python) -> fprintf env.py " ''' \n "; delayed () ;  fprintf env.py " \n r''' "
		| _         ->     delayed ()
	);
	end_compilation  = (fun  env -> output_string env.py "'''"; close_out env.py);
}


