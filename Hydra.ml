

let load f= 
 let cin = open_in f in
 let n= in_channel_length cin in
 let s=Bytes.create n in
 ignore(input cin s 0 n); Bytes.to_string s   

module M= Map.Make(String)

let backends = List.fold_left (fun set (x,y) -> M.add x y  set ) M.empty [ "python", Python.backend; "latex", Latex.backend ]  

exception Unknown_mode of string
let ()=  
	let ( !! )  = Printf.printf in
	try begin
	let mode = ref "latex" in		
	let spec = ["--mode", Arg.Set_string mode, "Define the output mode"] in
	let rpath = ref "" in
	let () = Arg.parse spec (fun path -> rpath:=path) "hydra --mode `mode source.hyd" in
	let path= !rpath in
	let basename = Filename.chop_extension path in
	let compile = try M.find !mode backends with
		| Not_found -> raise @@ Unknown_mode !mode   in 
	let ast =  load path |> Lexer.lex |> Parser.parse_hydra in
	compile basename ast 
 	end with
	| Unknown_mode mode -> 
		!! " The requested mode  `%s` does not correspond to any known mode. \n"  mode;
		!! " The available modes are: \n "; 
		M.iter (fun s _ -> !! " \t - %s\n" s) backends
	|Parser.Hydra_syntax_error s -> !! "There was a syntax error while parsing the input file. \n%s\n " s 






  
