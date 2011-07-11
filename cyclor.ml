let width, heigth = 650, 500;;


let colors = ref []
let color c = try List.assoc c !colors with Not_found -> failwith (c ^"inconnue");;

let affiche_carre (x, y) screen = 
  let r = Sdlvideo.rect x y 20 20 in
   Sdlvideo.fill_rect ~rect:r screen (color "red")
;;

let ($) f a = f a;;
let draw_cercle (x_c, y_c) r screen c =
  let x = ref 0 in
  let y = ref r in
  let m = ref (5-4*r) in
  let tracePixel (x, y) = Sdlvideo.put_pixel screen x y (color c) in
  while !x <= !y do
    tracePixel( !x+x_c, !y+y_c);
    tracePixel( !y+x_c, !x+y_c);
    tracePixel( -(!x)+x_c, !y+y_c);
    tracePixel( -(!y)+x_c, !x+y_c);
    tracePixel( !x+x_c, -(!y)+y_c);
    tracePixel( !y+x_c, -(!x)+y_c);
    tracePixel( -(!x)+x_c, -(!y)+y_c);
    tracePixel( -(!y)+x_c, -(!x)+y_c);
    if !m > 0 then (decr y; m:= !m-8*(!y));
    incr x; m:=!m+8*(!x)+4;
  done;
;;


type elec = {pos :int * int; vit : int * int; forces: int * int};;
let dt = 1;;
let r_elec = 10;;
let k = 5;;
let k' = 12;;
let k_repuls = 1000000;;
let l0 = 100;;
let sqrt_int x = int_of_float (sqrt $ float_of_int x);;
let ( ** ) k (x, y) = (k*x, k*y);;
let ( // ) (x, y) k = (x/k, y/k);;
let ( ++ ) (x, y) (x', y') = (x+x', y+y');;

let calcul_vp {pos = p; vit = v; forces = f} = 
  let v' = v ++ (dt ** f // 20) in
  let p' = p ++ (dt ** v' // 20)  in
  {pos = p'; vit = v'; forces = f}
;;

let dist (x, y) (x', y') = 
  let a, b = float_of_int (x-x'), float_of_int (y-y') in
  int_of_float (sqrt (a*.a +. b*.b))
;;

let centre_gravite elecs = 
  let e = List.map (fun e -> e.pos) elecs in
  let sum = List.fold_left (++) (List.hd e) (List.tl e) in
  sum // (List.length e)
;;
(* calcule les forces de cohésions relative à chaque paire d'électrons *)
let forces_elecs elecs = 
  let rec parcours l = function
    | [] -> []
    | e::reste -> 
      let ne = 
	List.fold_left 
	  (fun e e' ->
	    let x, y = e.pos and x', y' = e'.pos in
	    let d = match dist (x, y) (x', y') with 0 -> 1 | x -> x in
	    let i = (x'-x, y'-y) in
	    if d < 20 then {e with forces = e.forces ++ (-k_repuls)**i // (d*d*d)}
	    else {e with forces = e.forces ++ ((k*(d-l0))**i // d) ++ (-k')**e.vit }

	  ) 
	  {e with forces = (0, 0)} (l@reste) 
      in
      ne :: parcours (ne::l) reste
  in
  parcours [] elecs
;;


type block = { centre:int * int; spirit:string; force: elec->(int*int) };;

let lambda = 100000.;;
let a = 100;;

let make_mur vertical cpos = 
  {centre = cpos; spirit = if vertical then "murelectrover.bmp" else "murelectrohor.bmp"; 
   force = (fun e -> 
     let xe, ye =
       let x, y = e.pos in
       let xc, yc = cpos in
       if vertical then (x-xc, y-yc)
       else (y-yc, -(x-xc))
     in
     let xe', ye' = float_of_int xe, float_of_int ye in
     let a' = float_of_int a in
     (* 1/Sqrt[X^2 + (a - Y)^2] *)
     let a1 = 1./.sqrt(xe'*.xe' +. (a'-.ye')*.(a'-.ye')) in
     (* 1/Sqrt[X^2 + (a + Y)^2] *)
     let a2 =  1./.sqrt(xe'*.xe' +. (a'+.ye')*.(a'+.ye')) in
     let fx = (lambda *. (ye' *. (a2 -. a1) +. a' *. (a1 +. a2)))/.xe' in
     let fy =  lambda *. (a1 -. a2) in
       if vertical then (int_of_float fx, int_of_float fy) else (-int_of_float fy, int_of_float fx)
   )
      
  };;
let make_accelerateur cpos =
  let lx, ly = 40, 100 in
  {centre = cpos; spirit = "accelerateurver.bmp"; 
   force = (fun e -> 
     let xe, ye = e.pos ++ (-1)**cpos in
     if (abs xe < lx/2 && abs ye < ly/2) then (-3000, 0) else (0, 0)
   )
  }
;;

let blocks =  
  [ 
    make_accelerateur (460,350);
    make_accelerateur (460,450);

    make_accelerateur (440,350);
    make_accelerateur (440,450);
    make_accelerateur (420,350);
    make_accelerateur  (420,450);
   
    make_mur true (645, 400);
    make_mur false (550, 295); make_mur false (350, 295); 
    make_mur false (550, 495); make_mur false (350, 495); make_mur false (150, 495);
    make_mur true (100,400); make_mur true (100,200); 
    make_mur false (200, 100); make_mur false (400, 100);

    make_mur false (550, 5); make_mur false (350, 5); make_mur false (150, 5);
    make_mur true (645, 100)
    
  ];;



let e1 = {pos = (550,480); vit = (0, 0); forces = (0,0)};;
let e2 = {pos = (580, 450); vit = (0, 0); forces = (0,0)};;
let e3 = {pos = (550,440); vit = (0, 0); forces = (0,0)};;


let elecs = ref [e1; e2; e3];;

let next () = 
  elecs := forces_elecs (List.map calcul_vp !elecs);
  List.iter (fun b -> 
    elecs := List.map (fun e -> {e with forces = e.forces ++ b.force e}) !elecs
  ) blocks;
;;

let is_in_a_electron (xe, ye) (x, y) = 
  (x-xe)*(x-xe) + (y-ye)*(y-ye) < r_elec*r_elec
;;
let draw_electron p screen = 
  draw_cercle p 7 screen "grey";
  draw_cercle p 8 screen "black";
  draw_cercle p 9 screen "black";
  draw_cercle p 10 screen "grey"
;;

let affiche_block block screen ()=
  let s = Sdlvideo.load_BMP block.spirit in
  let l, h, _ = Sdlvideo.surface_dims s in
  let x, y = block.centre ++ (l, h)//(-2) in
  let r = Sdlvideo.rect x y 1 1 in
  Sdlvideo.blit_surface ~src:s ~dst:screen ~dst_rect:r ();
;;
let debug_elecs () = 
  List.iter (fun e -> Printf.printf "(%d, %d)" (fst e.pos) (snd e.pos)) !elecs;
  print_newline();
;;

let actions = Hashtbl.create 5;;

let view_scene screen =
  Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen Sdlvideo.white);
  List.iter (fun b -> affiche_block b screen ()) blocks;
  
  next ();

  Hashtbl.iter (fun _ f -> f screen) actions;

  List.iter (fun e -> 
    if fst e.pos >= 0 && snd e.pos > 0 && fst e.pos <= width && snd e.pos <= heigth then
     draw_electron (e.pos) screen
  ) 
    (!elecs); 
  for i = 0 to 3 do draw_cercle (centre_gravite (!elecs)) i screen "red" done;
  Sdlvideo.flip screen
;;

let time = ref 0;;

let rendu screen () = 
  let time' = Sdltimer.get_ticks() in
    if time' - !time >= 50 then
      begin
	view_scene screen;
	time := time';
      end;
  Sdltimer.delay 5
;;


let rec move_electron elec screen () = 
   match Sdlevent.wait_event () with
    | Sdlevent.MOUSEMOTION {Sdlevent.mme_which= _; 
			    Sdlevent.mme_state=(*[Sdlmouse.BUTTON_LEFT]*)_; 
			    Sdlevent.mme_x=x; Sdlevent.mme_y=y; 
			    Sdlevent.mme_xrel=_; Sdlevent.mme_yrel=_ }  ->
      let e = {elec with  pos = (x, y)} in
      elecs := e::!elecs;
       rendu screen ();
      elecs := List.tl !elecs;
      move_electron e screen ()
    | _ -> 
      print_endline "electron ajouté"; 
      elecs:= elec::!elecs; 
      Hashtbl.remove actions "cercle";
      debug_elecs (); 
      view_scene screen
;;


let rec control screen () = 
  match Sdlevent.poll () with
    | Some e ->
      begin match e with
	| Sdlevent.MOUSEBUTTONDOWN  { Sdlevent.mbe_x = x; Sdlevent.mbe_y = y; } 
	    when List.exists (fun elec -> is_in_a_electron  elec.pos (x, y)) (!elecs) -> 
	  let elec = List.find (fun elec -> is_in_a_electron elec.pos (x, y)) (!elecs) in
	  let g = centre_gravite !elecs in
	  Hashtbl.add actions "cercle" (fun s ->  draw_cercle g 100 s "red");
	  elecs := List.filter (fun e -> e <> elec) !elecs;
	  print_endline "clic de la sourie detecté";
	  rendu screen ();
	  move_electron elec screen (); control screen ();
	| Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_ESCAPE} ->
	  Sdl.quit ()
	| Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_DOWN} ->
	  elecs := List.map (fun e -> {e with pos = e.pos ++ (0, 5)}) !elecs;
	| Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_LEFT} ->
	  elecs := List.map (fun e -> {e with pos = e.pos ++ (-5, 0)}) !elecs;
	| Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_RIGHT} ->
	  elecs := List.map (fun e -> {e with pos = e.pos ++ (5, 0)}) !elecs;
	| Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_UP} ->
	  elecs := List.map (fun e -> {e with pos = e.pos ++ (0, -5)}) !elecs;
	| _ -> ()
      end
    | None ->()
;;

let loop screen () = 
  while true do
    rendu screen ();
    control screen ();
  done;
;;

let _ = 
  Sdl.init [`VIDEO]; (* only video *) 

  let (bpp, w, h) = (16, width, heigth) in
  let screen = Sdlvideo.set_video_mode ~w ~h ~bpp [`HWSURFACE] in
  Sdlkey.enable_key_repeat ~delay:10 ~interval:40 ();

  let toInt32 c = Sdlvideo.map_RGB screen c in
  colors := [("white", toInt32 Sdlvideo.white); 
	     ("red", toInt32 Sdlvideo.red);
	     ("black", toInt32 Sdlvideo.black); 
	     ("green", toInt32 Sdlvideo.green);
	     ("grey" , toInt32 (115, 115, 155))];
  Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen Sdlvideo.white);
  
  loop screen ();
;;
