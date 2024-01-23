open Graphics



(*type state = Normal | Invincible *)
module type Paddle =
sig
  type t (* Type float *)
  type colors (* Couleur du paddle *)
  type paddle (* Type du paddle *)

  (* Crée un paddle depuis une position, une taille, une couleur et un état *)
  val create : (t * t) -> (t * t) -> color -> paddle
  (* Renvoie la position du paddle *)
  val get_position : paddle -> (t * t)
  (* Renvoie la taille du paddle *)
  val get_size : paddle -> (t * t)
  (* Renvoie la couleur du paddle *)
  val get_color : paddle -> color
  (* Dessine le paddle *)
  (*val get_state : paddle -> state*)
  (* Retourner la valeur de x de la paddle *)
  val get_x : paddle -> t
  (* Retourner la valeur de y de la paddle *)
  val get_y : paddle -> t
  (* Retourner la valeur de width de la paddle *)
  val get_width : paddle -> t
  (* Retourner la valeur de height de la paddle *)
  val get_height : paddle -> t
  (* Change la position du paddle *)
  val set_position : paddle -> (t * t) -> paddle

  (* Change la taille du paddle *)
  val set_size : paddle -> (t * t) -> paddle
  (* Change la couleur du paddle *)
  val set_paddle_color : paddle -> color -> paddle

  val set_x : paddle -> t -> paddle

  val updatePadle : paddle -> (t*bool) -> paddle
  (* Change l'état du paddle *)
 (*val set_state : paddle -> state -> paddle*)
  (* Renvoie si la paddle est normal *)
  (*val is_normal : paddle -> bool
  (* Renvoie si la paddle est invincible *)
  val is_invincible : paddle -> bool*)
  (* Dessine le paddle *)
  val draw : paddle -> unit
end

module Paddle : Paddle with type t = float =
struct
  type t = float
  type colors = Graphics.color
  type paddle = (t * t) * (t * t) * colors 

  (* Getters *)
  let create pos size color  = (pos, size, color)
  let get_position (position, _, _) = position
  let get_size (_, size, _) = size
  let get_color (_, _, color) = color
  (*let get_state (_, _, _, state) = state*)
  let get_x (pos , _, _) = fst pos
  let get_y (pos, _, _) = snd pos
  let get_width (_, size, _) = fst size
  let get_height (_, size, _) = snd size

  (* Setters *)
  let set_position (_, size, color) new_position = (new_position, size, color)
  let set_size (position, _, color) new_size = (position, new_size, color)
  let set_paddle_color (position, size, _) new_color = (position, size, new_color)
 (* let set_state (position, size, color, _) new_state = (position, size, color, new_state) *)

  (* Retourner l'etat de la paddle *)
 (** let is_normal (_, _, _, state) = state = Normal
  let is_invincible (_, _, _, state) = state = Invincible*)

  (* Change la position du paddle *)
  let set_x (position, size, color) new_x = ((new_x, snd position), size, color)

  let updatePadle ((x, y),(width, height), color) (xs,buttonDown)  =
    let new_x =
      if buttonDown then
        max 0. (min (xs -. (width /. 2.)) (float_of_int (size_x ()) -. width))
      else x
    in
    ((new_x, y),(width, height), color)
    
                            
    

  (* Dessine le paddle *)
  let draw ((x, y),(width, height), color) =
    set_color color;
    fill_rect (int_of_float x) (int_of_float y) (int_of_float width) (int_of_float height)
end