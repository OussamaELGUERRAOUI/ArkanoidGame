open Graphics

(* Type de l'état de la balle *)
type state = Normal | Lost

module type Ball = sig
  type t
  type color = Graphics.color (* Couleur de la balle *)
  type ball (* Type de la balle *)

  (* Crée une balle depuis une position, un rayon, une couleur, un état et une vitesse *)
  val create : (t * t) -> t -> color -> state -> (t * t) -> ball
  (* Renvoie la position de la balle *)
  val get_position : ball -> (t * t)
  (* Renvoie la taille de la balle *)
  val get_radius : ball -> t
  (* Renvoie la couleur de la balle *)
  val get_color : ball -> color
  (* Renvoie l'état de la balle *)
  val get_state : ball -> state
  (* Renvoie la vitesse de la balle *)
  val get_speed : ball -> (t * t)
  (* Retourner la ppostion x de la balle *)
  val get_x : ball -> t
  (* Retourner la ppostion y de la balle *)
  val get_y : ball -> t
  (* Renvoie la balle après le changement de position *)
  val set_position : ball -> (t * t) -> ball
  (* Renvoie la balle après le changement de taille *)
  val set_radius : ball -> t -> ball
  (* Renvoie la balle après le changement de couleur *)
  val set_ball_color : ball -> color -> ball
  (* Renvoie la balle après le changement d'état *)
  val set_state : ball -> state -> ball
  (* Renvoie la balle après le changement de vitesse *)
  val set_speed : ball -> (t * t) -> ball


  

  val reflectBall : ball -> (t * t) -> (t * t) -> ball

  val updateBall : ball -> t -> ball


  (* Dessine la balle *)
  val draw : ball -> unit
end 


(* Avec type radius = float *)
module Ball : Ball with type t = float  = struct
  type t = float
  type color = Graphics.color
  type ball = (t * t) * t * color * state * (t * t)
  
  let create position radius color state speed = (position, radius, color, state, speed)
  
  let get_position (position,_,_,_,_) = position 
  let get_radius  (_,radius,_,_,_) = radius
  let get_color (_,_,color,_,_) = color
  let get_state (_,_,_,state,_) = state
  let get_speed  (_,_,_,_,speed) = speed
  let get_x ball = fst (get_position ball)
  let get_y ball = snd (get_position ball)
  
  let set_position (_, radius, color, state, speed) newPosition = (newPosition, radius, color, state, speed)
  let set_radius (position, _, color, state, speed) newradius = (position, newradius, color, state, speed)
  let set_ball_color (position, radius, _, state, speed) newColor = (position, radius, newColor, state, speed)
  let set_state (position, radius, color, _, speed) newState = (position, radius, color, newState, speed)
 
  let set_speed (position, radius, color, state, _) newSpeed = (position, radius, color, state, newSpeed)

  let reflectBall (position, radius, color, state, speed) (xp,yp) (width, height) = 
    let (x,y) = position in
    let (vx,vy) = speed in
    let nx =   if (x >= float_of_int (size_x ())) || x <= 0.  then
    -.vx
  (*else if ( y <= yp +. height +. 10.) && (x >= 0.) then
    if x <= (xp +. width) && x >= xp then
      -.vx
     else
      vx*)
  else
    vx in
    let ny = if (y <= (yp +. height) && x <= (xp +. width) && x >= xp)  || y >= float_of_int (size_y () ) then -. vy else vy in
    let newSpeed = (nx,ny) in
    (position, radius, color, state, newSpeed)

  
  let updateBall (position, radius, color, state, speed) dt =
    let (x,y) = position in
    let (vx,vy) = speed in
    let newX = x +. vx *. dt in
    let newY = y +. vy *. dt in
    let newPosition = (newX, newY) in
    (newPosition, radius, color, state, speed)
    

    
  let draw ((x,y), r, coloRb, _,_) =(
    set_color coloRb;
    fill_circle (int_of_float x) (int_of_float y) (int_of_float r) )
end


