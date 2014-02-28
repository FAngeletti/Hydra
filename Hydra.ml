
open Genlex



type hydre = Text of string | Python of string * hydres | PyInclusion of string | Section of string * hydres
and hydres = hydre list

let fold=List.fold_left

module Kset= Set.Make ( struct type t=string let compare= compare  end )
let keywords = ["#"; ":#"; "§"; ":§"; "<§" ; "§>" ]

open Printf 


let (!) f x y = f y x 

exception Syntax of string 

let kset = fold (!Kset.add) Kset.empty keywords 
let isKey s = Kset.mem s kset

module Charset= Set.Make ( struct type t=char  let compare= compare end ) 
let startChar= fold (fun set s -> Charset.add s.[0] set) Charset.empty keywords

let spaces = fold !Charset.add Charset.empty [' '; '\n'; '\t' ]



let py="§"
let c1, c2 = py.[0], py.[1]


type lex = T of string | Kwd of string*string | Skwd of string

let lex str=
let len =String.length str in
let advance pos error= if pos+1 >= len then raise (Syntax error) else pos +1 in
let (--) start pos = String.sub str start (pos-start) in
let extfactor pos = if ( pos < len && str.[pos]='>' )  then "§>" else "§" in
let extent pos = if str.[pos+1] = c1 then pos+3 else pos+2 in
let  extKeyword pos = match str.[pos] with
		| c1 when (pos+1< len) && str.[pos+1]=c2  -> extfactor (pos+2)
		| '#' -> "#"
		| _ -> pos -- (extent pos)    in


let extOption pos= 
	let rec ext start pos  = match str.[pos] with
		| ':' -> start -- pos |> String.trim , pos+1
		| _ -> advance pos ( sprintf "Searching for option starting from %d" start )  |> ext start in
	ext pos pos in  



let keyword pos = let k = extKeyword pos in
		if not(isKey k) then None else 
		match k with 
		| "§" | "#" -> let cont,pos' = extOption (pos+String.length k) in Some (pos', Kwd(cont, k) )
		| _ -> Some(pos+String.length k,Skwd k )
	 in

let rec text start pos  =
		let pos' = pos+1 in 
		if pos' >= len then
			[<' T(start -- pos) >]
		else if Charset.mem str.[pos'] startChar then
			match keyword pos' with
				| Some k ->  commit k start pos'
				| None ->  text start pos'
		else
			text start pos' 
and commit (pos',key) start pos =
	[< 'T(start--pos); 'key;  text pos' pos' >] in
text 0 0
 
let rec parse = parser 
	| [< 'T(s) ; r=parse >] -> Text(s)::r
	| [< 'Kwd(s,"#"); inter=parse; 'Skwd(":#"); r=parse >] -> Section(s, inter)::r
	| [< 'Kwd(opt,"§"); inter=parse; 'Skwd(":§"); r=parse >] -> Python(opt, inter)::r
	| [< 'Skwd("<§"); inter=text; 'Skwd("§>"); r=parse >] -> PyInclusion(inter)::r
	| [< >] -> []
and text = parser
	|[< 'T(s) >] -> s
	| [< >] -> ""


let rec sprint hydres = String.concat ";" ( List.map sprintEl hydres ) 
and sprintEl= function
| Text(s) -> sprintf "Text(%s)\n"  s 
| Section(name,hs) ->  sprintf "Section<%s>(\n %s \n )" name (sprint  hs)  
| Python(cont,hs) -> sprintf "Python<%s>(%s)\n " cont (sprint hs) 
| PyInclusion(s) -> sprintf "Inclusion(%s)\n" s
   


let preambule chan latexname =
fprintf chan 
"#!/usr/bin/env python 
#This is an automatically generated python generator file.
__latexFile__=open('%s', 'w+')

def __latex__(str):
	__latexFile__.write(str)

def __inclusion__(obj):
	try:
		s=obj.latex()
	except AttributeError:
		s=str(obj)
	__latex__(s)


" latexname

module SMap = Map.Make(String)


type env= { mutable ntab : int ; pyx: out_channel; chans : out_channel SMap.t }

let ( |: ) name env = try SMap.find name env.chans with
| Not_found ->  open_out name

let select env name =
let (chan, env') = 
	try (SMap.find name env.chans, env ) with
	| Not_found -> let chan= open_out name in  
	               let chans = env.chans |> SMap.add name chan |> SMap.add "" chan in
			chan, {env with chans}
	in
env'   
	



let pygenName s =   "gen_"^s^".py" 
let latexName s =   s^".tex" 


let init_env basename = 
let pyx = pygenName basename |> open_out in
let chans = SMap.empty |> SMap.add "X" pyx |> SMap.add "" pyx in
latexName basename |> preambule pyx;
{ntab=0;pyx;chans}

let count_tab s = 
let rec aux n = match s.[n] with
|'\t' -> 1+ aux (n-1) 
| _ -> 0 in
(String.length s) -1  |> aux 

let latexW chan  =  fprintf chan "__latex__(r'''%s''')\n"  
let pythonW chan s = String.trim s |> fprintf chan "%s \n"
let inpythonW ntab chan = 
for i=1 to ntab do
 output_char chan '\t' 
done
;fprintf chan "__inclusion__(%s)\n"

let rec  transf mode env hydres  = List.iter (mode env) hydres 
and latex env = function
| Text(s) ->  latexW env.pyx s
| Section(name, hydres) ->   transf  latex (select env name) hydres  
| Python(cont, hydres) ->  transf python (select env cont)  hydres
| PyInclusion(s) -> inpythonW 0 env.pyx s 
and python env = function
| Text(s) ->  env.ntab<- count_tab s;  pythonW ( "" |: env ) s
| Section(name,hydres) -> transf latex (select env name) hydres 
| Python(cont, hydres) ->  transf python (select env cont)  hydres
| PyInclusion(s) -> inpythonW env.ntab env.pyx s 



let load f= 
 let cin = open_in f in
 let n= in_channel_length cin in
 let s=String.create n in
 ignore(input cin s 0 n); s   


let ()=  		
	let path=Sys.argv.(1) in
	let basename = Filename.chop_extension path in
	let gen = pygenName basename in 
	let env = init_env basename in
	load path |> lex |> parse |> transf latex env; close_out env.pyx;  match Sys.command ("./"^gen) with
		| 0 -> ()
		| _ -> ()
	 







  
