

let load f= 
 let cin = open_in f in
 let n= in_channel_length cin in
 let s=Bytes.create n in
 ignore(input cin s 0 n); Bytes.to_string s   

module M= Map.Make(String)

let frontends = List.fold_left (fun set (x,y) -> M.add x y  set ) M.empty [ "python", Lexer.py_aut; "generic", Lexer.gen_a ]  

let backends = List.fold_left (fun set (x,y) -> M.add x y  set ) M.empty [ "python", Python.backend; "latex", Latex.backend ]  

exception Unknown_mode of string
exception Unknown_option of string*string


let ()=  
	let ( !! )  = Printf.printf in
	try begin
	let syntax = ref "python" in
	let mode = ref "latex" in		
	let spec = ["--mode", Arg.Set_string mode, "Define the template engine mode"; "--syntax", Arg.Set_string syntax, "Define the template syntax variant"  ] in
	let rpath = ref "" in
	let () = Arg.parse spec (fun path -> rpath:=path) "hydra --mode `mode --syntax `syn source.hyd" in
	let path= !rpath in
	let basename = Filename.chop_extension path in
	let compile = try M.find !mode backends with
		| Not_found -> raise @@ Unknown_option ("mode",!mode)   in 
	let lexer = try Lexer.lex @@ M.find !syntax frontends with 
		|Not_found ->  raise @@ Unknown_option ("syntax",!syntax) in
	let ast =  load path |> lexer |> Parser.parse_hydra in
	compile basename ast 
 	end with
	| Unknown_option (name, opt)  -> begin
		!! " The requested %s  `%s` does not correspond to any known %s. \n"  name opt name;
		!! " The available %s are: \n " name;
		match name with 
			| "syntax" ->   M.iter (fun s _ -> !! " \t - %s\n" s) frontends
			| "mode" -> M.iter (fun s _ -> !! " \t - %s\n" s) backends
			| _ -> !!"?"
		end	
	|Parser.Hydra_syntax_error s -> !! "There was a syntax error while parsing the input file. \n%s\n " s 






  
