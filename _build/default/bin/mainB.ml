
open Iterator
open Brick
open Padle
open Ball
(* Définir les modules Brick et Flux ici *)

module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

type game_state = {
  bricks : Brick.brick list list;
  paddle : Paddle.paddle;
  ball : Ball.ball;
}



let draw_state (state : game_state) =
  let bricks, padle, ball = state.bricks, state.paddle, state.ball in
  (*let get_click_position () =
  let event = wait_next_event [Button_down] in
    (event.button,(float_of_int event.mouse_x, float_of_int event.mouse_y)) in
    
  let _, posM = get_click_position () in
  let new_brick = Brick.update_brick_lines posM bricks in *)
  let score = Brick.nbBricks bricks in
  Graphics.moveto 10 10;  (* Déplace le pointeur en bas à gauche *)
  Graphics.draw_string ("Score: " ^ (string_of_int score));
  Brick.draw_brick_lines bricks;
  Paddle.draw padle;
  Ball.draw ball





  let update_game (game : game_state) =
    let ball =  game.ball in
    let paddle = game.paddle in
    let new_ball = Ball.updateBall ball Init.dt in
    let (x,_) = Graphics.mouse_pos () in
    let new_paddle = Paddle.updatePadle paddle (float_of_int(x),true) in
    { game with ball = new_ball ; paddle = new_paddle}

    
  let rec loop game flux_etat =
      Graphics.clear_graph ();
      draw_state game;
      Graphics.synchronize ();
      Unix.sleepf 0.03;
      match Flux.uncons flux_etat with
      | None -> ()
      | Some (_, flux_etat') ->
        let newGame = update_game game in
        loop newGame flux_etat'

  let draw game flux_etat =
    Graphics.open_graph graphic_format;
    Graphics.auto_synchronize false;
    loop game flux_etat;
    Graphics.close_graph ()
       
        
      

let () =
  let posd = (50., 300.) in
  let posM = (Box.supx , Box.supy ) in
 
  
  let coleR = Graphics.blue in
  let coloRpadle = Graphics.black in
  let coloRball = Graphics.red in
  let paddleInit = Paddle.create (300.,60.) (90.,20.) coloRpadle in
  let ballInit = Ball.create (300.,150.) 5. coloRball Normal (0.4,0.5) in
  
  let initial_state : game_state = { bricks = Brick.generate_brick_lines posd posM coleR;
  paddle = paddleInit;
  ball = ballInit } in

  let flux_etat = Flux.unfold (fun state -> Some (state, update_game state)) initial_state in
  
  draw initial_state flux_etat;

