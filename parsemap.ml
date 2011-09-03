let blocks : (string,  int * int -> unit) Hashtbl.t = Hashtbl.create 10;;
let add_block name fct = Hashtbl.add blocks name fct;;

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

let parse file = 
  let rec parse' = function
    | [] -> [] 
    | [x; y; t]:: r -> 
      let pos = (int_of_string x, int_of_string y) in
      let obj = 
	if Hashtbl.mem blocks t then (t, pos)
	else  failwith ("objet "^t^" non connu")
      in
      obj::parse' r
    | _::r -> parse' r
  in
  parse' (List.map (fun s -> string_split s ' ') (string_split (read file) '\n'))
;;


let view map () = List.iter (fun (id, pos) -> let f = Hashtbl.find blocks id in f pos) map;;


let charge map = 
  view (parse map) ()
