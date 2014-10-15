

let load f= 
 let cin = open_in f in
 let n= in_channel_length cin in
 let s=Bytes.create n in
 ignore(input cin s 0 n); Bytes.to_string s   

module M= Map.Make(String)

let frontends = List.fold_left (fun set (x,y) -> M.add x y  set ) M.empty [ "python", Lexer.py_aut; "generic", Lexer.gen_a ]  

let backends = List.fold_left (fun set (x,y) -> M.add x y  set ) M.empty [ "python", Python.backend; "latex", Latex.backend; "ocaml_i", Ocaml_i.backend  ]  

exception Unknown_mode of string
exception Unknown_option of string*string

let suffix s=
	let rec dot_pos k= if s.[k] = '.' || k=0 then k else dot_pos (k-1) in
	let last = String.length s  - 1in
	let pos = dot_pos @@ last in
	String.sub s (pos + 1) @@ last - pos 
 

let extension_hints = function
	| "hytex" -> Some ( "python"), Some( "latex" )
	| "hyml" ->  Some( "generic" ) , Some ("ocaml_i" )
	| _ -> None, None

let merge_hint_option a b default=
	match !a, b with
		| "", Some h -> a:= h
		| "", None -> a:=default
		| _ -> ()

let ()=  
	let ( !! )  = Printf.printf in
	try begin
	let syntax = ref "" in
	let mode = ref "" in		
	let spec = ["--mode", Arg.Set_string mode, "Define the template engine mode"; "--syntax", Arg.Set_string syntax, "Define the template syntax variant"  ] in
	let rpath = ref "" in
	let () = Arg.parse spec (fun path -> rpath:=path) "hydra --mode `mode --syntax `syn source.hyd" in
	let path= !rpath in
	let basename = Filename.chop_extension path
	and extension = suffix path in
	let hint_syntax, hint_mode = extension_hints extension in
	let () = 
		merge_hint_option syntax hint_syntax "python" ;
		merge_hint_option mode hint_mode "latex"  in
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

  
