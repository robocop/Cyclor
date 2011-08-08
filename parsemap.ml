type block = 
  | Electron of (int * int)
  | Accelerateurver of (int * int)
  | Wall of bool * (int * int)
  | Inf_magnetic_fil of bool * (int * int)
;;

let string_split string separator = 
  let rec aux old c =
    try
      match string.[c] with
	| char when char = separator ->
	  old :: aux "" (c+1)
	| char -> aux (old^(String.make 1 char)) (c+1)

    with Invalid_argument _ -> [old]
  in
  aux "" 0
;;

let read file =
  let ich = open_in file in
  let len = in_channel_length ich in
  let str = String.create len in
  really_input ich str 0 len;
  close_in ich;
  str
;;
let s = read "map";;
let parse file = 
  let rec parse' = function
    | [] -> [] 
    | [x; y; t]:: r -> 
      let pos = (int_of_string x, int_of_string y) in
      let obj = match t with
	| "murelectrover" ->  Wall(true, pos)
	| "murelectrohor" ->  Wall(false, pos)
	| "filmagn_bas" -> Inf_magnetic_fil(true, pos)
	| "filmagn_haut" -> Inf_magnetic_fil(false, pos)
	| "accelerateurver" -> Accelerateurver pos
	| "electron" -> Electron pos
	| obj -> failwith ("objet "^obj^" non connu")
      in
      obj::parse' r
    | _::r -> parse' r
  in
  parse' (List.map (fun s -> string_split s ' ') (string_split (read file) '\n'))
;;

parse "map";;
