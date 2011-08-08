let ($) f a = f a;;
let ( ** ) k (x, y) = (k*x, k*y);;
let ( // ) (x, y) k = (x/k, y/k);;
let ( ++ ) (x, y) (x', y') = (x+x', y+y');;
let ( -- ) (x, y) (x', y') = (x-x', y-y');;
let sqrt_int x = int_of_float (sqrt $ float_of_int x);;

module Const =
   struct
     let width, heigth = 650, 500
       (** Nombre de frames par secondes = 1/dt (dt en ms) **)
     let fps = 50
     let dt = 1000/fps
     let k_elec_repuls = 100000
     let k_elec_spring = 5
     let l0_elec = 40
     let k_elec_shock_absorber = 12
     let r_elec = 10

     let wall_half_length = 100
     let k_wall = 100000
   end;;

module Bat_Sdl = 
  struct
    let colors = ref []
    let color c = try List.assoc c !colors with Not_found -> failwith (c ^" unknow")

    let draw_pixel (x, y) c screen = 
      if x >= 0 && y >= 0 && x < Const.width && y < Const.heigth then 
	Sdlvideo.put_pixel screen x y (color c)
    let draw_cercle (x_c, y_c) r c screen =
      let x = ref 0 in
      let y = ref r in
      let m = ref (5-4*r) in
      while !x <= !y do
	draw_pixel (!x+x_c, !y+y_c) c screen;
	draw_pixel (!y+x_c, !x+y_c) c screen;
	draw_pixel (-(!x)+x_c, !y+y_c) c screen;
	draw_pixel (-(!y)+x_c, !x+y_c) c screen;
	draw_pixel (!x+x_c, -(!y)+y_c) c screen;
	draw_pixel (!y+x_c, -(!x)+y_c) c screen;
	draw_pixel (-(!x)+x_c, -(!y)+y_c) c screen;
	draw_pixel (-(!y)+x_c, -(!x)+y_c) c screen;
	if !m > 0 then begin decr y; m:= !m-8*(!y) end;
	incr x; m:=!m+8*(!x)+4;
      done
  end;;
module Electrons = 
  struct
    type elec = {pos :int * int; vit : int * int; forces: int * int; active:bool}

    (** Liste des electrons **)
    let elecs = ref []
    let create pos = 
      elecs := {pos =pos; vit = (0, 0); forces = (0,0); active = false}::!elecs
    ;;
    let calcul_vp {pos = p; vit = v; forces = f; active = b} = 
      let v' = v ++ (f // Const.fps) in
      let p' = p ++ (v' // Const.fps)  in
      {pos = p'; vit = v'; forces = f; active = b}
    let centroid elecs = 
      let e = List.map (fun e -> e.pos) elecs in
      let sum = List.fold_left (++) (List.hd e) (List.tl e) in
      sum // (List.length e)
    let dist (x, y) (x', y') = 
      let a, b = float_of_int (x-x'), float_of_int (y-y') in
      int_of_float (sqrt (a*.a +. b*.b))
    let is_in_a_electron (xe, ye) (x, y) = 
      (x-xe)*(x-xe) + (y-ye)*(y-ye) < Const.r_elec*Const.r_elec

    (** calcule les forces de cohésions relative à chaque paire d'électrons **)
    let forces_elecs () = 
      let rec iter l = function
	| [] -> []
	| e::reste -> 
	  let ne = 
	    List.fold_left 
	      (fun e e' ->
		let x, y = e.pos and x', y' = e'.pos in
		let d = match dist (x, y) (x', y') with 0 -> 1 | x -> x in
		let i = (x'-x, y'-y) in
		if d < 2*Const.r_elec then {e with forces = e.forces ++ (-Const.k_elec_repuls)**i // (d*d*d)}
		else 
		  {e with forces = e.forces 
		     ++  ((Const.k_elec_spring*(d-Const.l0_elec))**i // d) 
		     ++ (-Const.k_elec_shock_absorber)**e.vit 
		  }
		  
	  ) 
	      {e with forces = (0, 0)} (l@reste) 
	  in
	  ne :: iter (ne::l) reste
      in
      elecs := iter [] !elecs
    let calcul_pos () = 
	elecs := List.map calcul_vp !elecs
    let decallage () = 
      centroid !elecs -- (Const.width/2, Const.heigth/2)
    let draw_electron e screen =
      let e = {e with pos = e.pos -- decallage()} in
      let c1, c2 = if not e.active then "grey", "black" else "red", "red" in
      Bat_Sdl.draw_cercle e.pos 7 c1 screen;
      Bat_Sdl.draw_cercle e.pos 8 c2 screen;
      Bat_Sdl.draw_cercle e.pos 9 c2 screen;
      Bat_Sdl.draw_cercle e.pos 10 c1 screen

    let draw screen = 
      List.iter (fun e -> draw_electron e screen)  !elecs;
	for i = 0 to 3 do Bat_Sdl.draw_cercle (centroid !elecs -- decallage()) i "red"  screen done;
  end;;

module Blocks = 
struct
  type block = { centre:int * int; spirit:string; force: Electrons.elec->(int*int) }
  let blocks = ref []

  let make_wall vertical cpos = 
    blocks := {centre = cpos; spirit = if vertical then "murelectrover.bmp" else "murelectrohor.bmp";
     force = (fun e -> 
       let xe, ye =
	 let x, y = e.Electrons.pos in
	 let xc, yc = cpos in
	 if vertical then (x-xc, y-yc)
	 else (y-yc, -(x-xc))
       in
       let xe', ye' = float_of_int xe, float_of_int ye in
       let a' = float_of_int Const.wall_half_length  in
       (* 1/Sqrt[X^2 + (a - Y)^2] *)
       let a1 = 1./.sqrt(xe'*.xe' +. (a'-.ye')*.(a'-.ye')) in
       (* 1/Sqrt[X^2 + (a + Y)^2] *)
       let a2 =  1./.sqrt(xe'*.xe' +. (a'+.ye')*.(a'+.ye')) in
       let fx = ((float_of_int Const.k_wall) *. (ye' *. (a2 -. a1) +. a' *. (a1 +. a2)))/.xe' in
       let fy =  (float_of_int Const.k_wall) *. (a1 -. a2) in
       if vertical then(int_of_float fx, int_of_float fy) 
       else (-int_of_float fy, int_of_float fx)
     )
	      } :: !blocks
  ;;
  let make_accelerator cpos =
    let lx, ly = 40, 100 in
    blocks := {centre = cpos; spirit = "accelerateurver.bmp"; 
     force = (fun e -> 
       let xe, ye = e.Electrons.pos -- cpos in
       if (abs xe < lx/2 && abs ye < ly/2) then (-3000, 0) else (0, 0)
     )
    } :: !blocks
  let make_inf_magnetic_fil bas cpos = 
    let lambda = 300000 in
    let sign = if bas then 1 else (-1) in
    blocks := {centre = cpos; spirit = if bas then "filmagn_bas.bmp" else "filmagn_haut.bmp"; force = 
	(fun e ->
	  let xe, ye = e.Electrons.pos -- cpos in
	  let d = Electrons.dist cpos e.Electrons.pos in
	  let dir = (-ye, xe) in
	  ((sign*lambda) ** dir) // (d*d) 
	)
    } ::!blocks

     (*
  let cyclo_times = Hashtbl.create 5
  let make_cyclotron cpos = 
    let c1, c2 = cpos -- (90, 0), cpos ++ (90, 0) in
    let periode = 2000 in
    Hashtbl.add cyclo_times cpos (true, Sdltimer.get_ticks ());
    let lambda = 200000 in
    blocks := {
      centre = cpos; 
      spirit = "cyclotron.bmp";
      force = (fun e ->
	let xe1, ye1 =  e.Electrons.pos -- c1 and xe2, ye2 =   e.Electrons.pos -- c2 in
	let d1, d2 = Electrons.dist c1 e.Electrons.pos, Electrons.dist c2 e.Electrons.pos in
	let dir1, dir2 = (-ye1, xe1), (-ye2, xe2) in
	let b, t = Hashtbl.find cyclo_times cpos in
	let t' = Sdltimer.get_ticks() in
	if t' - t > periode then begin Hashtbl.replace cyclo_times cpos (not b, t') end;
	if b then ((lambda) ** dir1) // (d1*d1) else ((lambda) ** dir2) // (d2*d2)
      )
	
    } ::!blocks

     *)

  let make_unif cpos = 
    let lambda = 33 in
    blocks := {
      centre = cpos; 
      spirit = "champs_magn_unif.bmp";
      force = (fun e ->
	let xe, ye = e.Electrons.pos -- cpos in
	let d = Electrons.dist (xe, ye) (0,0) in
	if d < 150 then
	  let x', y' = e.Electrons.vit in (lambda*y', -lambda*x')
	else (0,0)
      )
	
    } ::!blocks
  let draw_block block screen =
    let block = {block with centre = block.centre  --Electrons.decallage()} in
    let s = Sdlvideo.load_BMP block.spirit in
    let w, h, _ = Sdlvideo.surface_dims s in
    let x, y = block.centre ++ (w, h)//(-2) in
    let r = Sdlvideo.rect x y 1 1 in
    Sdlvideo.blit_surface ~src:s ~dst:screen ~dst_rect:r ()
  let draw screen = 
    List.iter (fun b -> draw_block b screen) !blocks
end;;

module Checkpoints =
struct
  type typec = D | A | C
  type check = { typec:typec; centre_c: int*int; vert:bool; mutable is_active:bool; is_in:Electrons.elec list-> bool }
  let checkpoints = ref []
  let make_check typec cpos vertical = 
    let w, h = if vertical then 10,100 else 100,10 in
    checkpoints := 
      { 
	typec=typec; 
	centre_c=cpos; 
	is_active=false;
	vert=vertical;
	is_in= (
	  fun elecs ->
	    let xe, ye = (Electrons.centroid elecs) -- cpos in
	    abs xe < w/2 && abs ye < h/2
	)
      }::!checkpoints
  let draw_check check screen = 
    let check = {check with centre_c = check.centre_c -- Electrons.decallage()} in 
    let c = match check.typec, check.is_active with
      | D, false -> "green"
      | D, true -> "hgreen"
      | C, false -> "yellow"
      | C, true -> "hyellow"
      | A, false -> "red"
      | A, true -> "hred"
    in
    let h, w = if check.vert then 100,10 else 10, 100 in
    let x, y = check.centre_c ++ (w, h)//(-2) in
    let r = Sdlvideo.rect x y w h in
    Sdlvideo.fill_rect ~rect:r screen (Bat_Sdl.color c)
  ;;
  let draw screen = 
    List.iter (fun c -> draw_check c screen) !checkpoints
  let active elecs = 
     List.iter (fun c -> if c.is_in elecs then (c.is_active <- true)) !checkpoints
end;;
module Rendu = 
struct
  let time = ref 0
  let actions = Hashtbl.create 5
  let next () = 
    Electrons.calcul_pos();
    Electrons.forces_elecs ();
    (** Ajout de la force créés par les blocks à chaque électron **)
    List.iter (fun b -> 
      Electrons.elecs := List.map (fun e -> {e with Electrons.forces = e.Electrons.forces ++ b.Blocks.force e}) !Electrons.elecs
    ) !Blocks.blocks;
    
    Checkpoints.active !Electrons.elecs
  ;;
  let view_scene screen = 
    Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen Sdlvideo.white); 
    Blocks.draw screen;
    Checkpoints.draw screen;
    next ();

    Hashtbl.iter (fun s f -> (ignore s.[0]; f screen)) actions;
    
    Electrons.draw screen;
    Sdlvideo.flip screen
  let rendu screen = 
    let time' = Sdltimer.get_ticks() in
    if time' - !time >= Const.dt then
      begin
	view_scene screen;
	time := time';
      end;
    Sdltimer.delay 5

end;;

module Controles =
struct
  let rec move_electron elec screen = 
    match Sdlevent.wait_event () with
      | Sdlevent.MOUSEMOTION {Sdlevent.mme_which= _; 
			      Sdlevent.mme_state=_; 
			      Sdlevent.mme_x=x; Sdlevent.mme_y=y; 
			      Sdlevent.mme_xrel=_; Sdlevent.mme_yrel=_ }  ->
	let x, y = (x, y) ++ Electrons.decallage() in
	let g = Electrons.centroid !Electrons.elecs in
	let d = Electrons.dist g (x, y) in
	let x, y = if d > 100 then g ++ 100 **((x, y) ++ (-1)**g) // d else (x, y) in
	let e = {elec with  Electrons.pos = (x, y); Electrons.active = true} in
	Electrons.elecs := e::!Electrons.elecs;
	Rendu.rendu screen;
	Electrons.elecs := List.tl !Electrons.elecs;
	move_electron e screen
      | _ -> 
	Electrons.elecs:= {elec with Electrons.active = false}::!Electrons.elecs; 
	Hashtbl.remove Rendu.actions "cercle"

  let dir direction vect = 
     Hashtbl.add Rendu.actions direction
       (fun s -> Electrons.elecs := List.map (fun e -> {e with Electrons.pos = e.Electrons.pos ++ vect}) !Electrons.elecs)
  ;;
  let rec control screen = 
    match Sdlevent.poll () with
      | Some e ->
	begin match e with
	  | Sdlevent.MOUSEBUTTONDOWN  { Sdlevent.mbe_x = x; Sdlevent.mbe_y = y; } 
	      when List.exists (fun elec -> Electrons.is_in_a_electron  (elec.Electrons.pos--Electrons.decallage()) (x, y)) (!Electrons.elecs) -> 
	    let elec = List.find (fun elec -> Electrons.is_in_a_electron (elec.Electrons.pos--Electrons.decallage()) (x, y)) (!Electrons.elecs) in
	    
	    Hashtbl.add Rendu.actions "cercle" 
	      (fun s -> Bat_Sdl.draw_cercle (Electrons.centroid !Electrons.elecs --Electrons.decallage()) 100 "red" s);
	    Electrons.elecs := List.filter (fun e -> e <> elec) !Electrons.elecs;
	    Rendu.rendu screen;
	    move_electron elec screen; control screen;
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_DOWN} -> dir "down" (0,5);
	  | Sdlevent.KEYUP {Sdlevent.keysym=Sdlkey.KEY_DOWN} -> Hashtbl.remove Rendu.actions "down";
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_LEFT} -> dir "left" (-5,0);
	  | Sdlevent.KEYUP {Sdlevent.keysym=Sdlkey.KEY_LEFT} -> Hashtbl.remove Rendu.actions "left";
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_RIGHT} -> dir "right" (5,0)
	  | Sdlevent.KEYUP {Sdlevent.keysym=Sdlkey.KEY_RIGHT} -> Hashtbl.remove Rendu.actions "right";
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_UP} -> dir "up" (0,-5)
	  | Sdlevent.KEYUP {Sdlevent.keysym=Sdlkey.KEY_UP} -> Hashtbl.remove Rendu.actions "up";
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_ESCAPE} -> Sdl.quit (); exit 0;
	  | _ -> ()
	end
      | None ->()
end


open Parsemap
let charge map = 
  let f = function
    | Electron pos -> Electrons.create pos
    | Accelerateurver pos -> Blocks.make_accelerator pos
    | Wall (b, pos) -> Blocks.make_wall b pos
    | Inf_magnetic_fil (b, pos) ->  Blocks.make_inf_magnetic_fil b pos
  in
  List.iter f (parse map)


let _ = 
  Sdl.init [`VIDEO]; 

  let (bpp, w, h) = (16, Const.width, Const.heigth) in
  let screen = Sdlvideo.set_video_mode ~w ~h ~bpp [`HWSURFACE] in

  let toInt32 c = Sdlvideo.map_RGB screen c in
  Bat_Sdl.colors := [("white", toInt32 Sdlvideo.white); 
	     ("red", toInt32 Sdlvideo.red);
	     ("black", toInt32 Sdlvideo.black); 
	     ("green", toInt32 Sdlvideo.green);
	     ("grey" , toInt32 (115, 115, 155));
	     ("yellow", toInt32 (255,252,27));
	     ("hgreen", toInt32 (35,122,29));
	     ("hyellow", toInt32 (213,211,82));
	     ("hred", toInt32 (178,30,30))
	    ];

  (*
  Electrons.create (550, 400);
  Electrons.create (570,420);

  Blocks.make_accelerator (460,350); Blocks.make_accelerator (460,450);
  Blocks.make_accelerator (440,350);Blocks.make_accelerator (440,450);
  Blocks.make_accelerator (420,350); Blocks.make_accelerator  (420,450);
  Blocks.make_accelerator (400,350); Blocks.make_accelerator  (400,450);
  Blocks.make_accelerator (380,350);Blocks.make_accelerator (380,450);
  Blocks.make_accelerator (360,350);Blocks.make_accelerator  (360,450);
  Blocks.make_accelerator (340,350);Blocks.make_accelerator  (340,450);
  Blocks.make_accelerator (320,350);Blocks.make_accelerator  (320,450);
  Blocks.make_accelerator (300,350);Blocks.make_accelerator  (300,450);
  Blocks.make_accelerator (280,350);Blocks.make_accelerator  (280,450);
  Blocks.make_accelerator (260,350);Blocks.make_accelerator  (260,450);
  Blocks.make_accelerator (240,350);Blocks.make_accelerator  (240,450);
  Blocks.make_wall true (645, 400);
  Blocks.make_wall false (550, 295); Blocks.make_wall false (350, 295); 
  Blocks.make_wall false (550, 495); Blocks.make_wall false (350, 495); Blocks.make_wall false (150, 495);
  Blocks.make_wall true (100,400); Blocks.make_wall true (100,200); 
  Blocks.make_wall false (200, 100); Blocks.make_wall false (400, 100);

  Blocks.make_wall false (550, 5); Blocks.make_wall false (350, 5); Blocks.make_wall false (150, 5);
  Blocks.make_wall true (645, 100);
  Blocks.make_inf_magnetic_fil false (500, 100);
  Blocks.make_inf_magnetic_fil false (130,400);
  Blocks.make_inf_magnetic_fil true (400,200);

  Checkpoints.make_check Checkpoints.D (500, 400) true;
  Checkpoints.make_check Checkpoints.C (155, 300) false;
  Checkpoints.make_check Checkpoints.C (350, 155) true;
  Checkpoints.make_check Checkpoints.A (100, 50) true;


  Blocks.make_unif (-300,30);

  *)
  charge "map1";
  while true do
    Rendu.rendu screen;
    Controles.control screen;
  done;
