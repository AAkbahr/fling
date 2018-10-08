type direction = Up | Right | Down | Left

type ball = int * Position.t

type move = ball * direction

type game = G of ball list

let id = ref 0

    (* BUILDING FUNCTIONS *)
let make_ball p =
  let b = (!id, p) in begin
    id := !id+1;
    b
  end

let new_game ps = G(ps)

let eq_ball b b' = match (b, b') with
  | ((id1, p1), (id2,p2)) -> (id1 = id2)

let get_balls g = match g with
  | G(bl) -> bl

    (* DEBUGGING FUNCTIONS *)
let debug_mode = true

let debug s =
  if debug_mode then print_string s else ()

let string_of_direction d = match d with
  | Up -> "U"
  | Down -> "D"
  | Left -> "L"
  | Right -> "R"

let print_game game = begin
  debug "balls: [" ;
  let rec aux balls = match balls with
    | [] -> debug "]\n"
    | ((id,pos))::t -> debug ((string_of_int id) ^ (Position.string_of_position pos) ^ ";") ; aux t
  in aux (get_balls game)
end

let print_moves game_moves = begin
  debug "moves: [" ;
  let rec aux game_moves = match game_moves with
    | [] -> debug "]\n"
    | (((id,pos),dir))::t -> debug ((string_of_int id) ^ (string_of_direction dir) ^ ";") ; aux t
  in aux game_moves
end

    (* OTHER FUNCTIONS *)

let ball_of_position game p =
  let rec aux bl p = match bl with
    | []   -> failwith "No ball on this position"
    | ((id,pos))::t -> if Position.eq p pos then (id,pos) else aux t p
  in aux (get_balls game) p

let position_of_ball b = match b with
  | (id, p) -> p

let is_ball g p =
  let rec aux bl p = match bl with
    | []                -> false
    | ((id,pos))::t -> if Position.eq p pos then true else aux t p
  in aux (get_balls g) p

let make_move p d = (p, d)

let moves g =
  let rec aux bl acc = match bl with
    | []    -> acc
    | h::t  -> let rec search_ball h balls acc = match balls with
      | []      -> acc
      | h1::t1  -> let p = (position_of_ball h) and p1 = (position_of_ball h1) in
        let (x,y) = (Position.proj_x p, Position.proj_y p) in
        match (Position.proj_x p1, Position.proj_y p1) with

        | (x1,y1) when (x=x1 && y < y1-1) -> search_ball h t1 ((make_move h Up)::(make_move h1 Down)::acc)

        | (x1,y1) when (x=x1 && y > y1+1) -> search_ball h t1 ((make_move h1 Up)::(make_move h Down)::acc)

        | (x1,y1) when (y=y1 && x < x1-1) -> search_ball h t1 ((make_move h Right)::(make_move h1 Left)::acc)

        | (x1,y1) when (y=y1 && x > x1+1) -> search_ball h t1 ((make_move h1 Right)::(make_move h Left)::acc)

        | _ -> search_ball h t1 acc
    in aux t (search_ball h t acc)
  in aux (get_balls g) []

let apply_move g move =
  let (b,dir) = move in
  let rec apply_move_aux g b dir first =
    let game_moves = moves g in
    print_game g;
    print_moves game_moves;
    if List.mem (make_move b dir) game_moves then
      let rec get_ball_to_move g b dir balls acc =
        match balls with
        | [] -> failwith "No ball to move"
        | h::t when eq_ball h b -> debug " > found ball to move" ; find_pos g b dir (acc @ balls)
        | h::t -> get_ball_to_move g b dir t (h::acc)

      and find_pos g b dir balls =
        let (id,p) = b in
        let rec find_new_pos g f p balls = match f p with
          | pos when (is_ball g pos) -> debug " > found ball to kill" ; let b1 = ball_of_position g pos in apply_move_aux (new_game ((id,p)::(remove_ball b balls []))) b1 dir false
          | pos -> debug (Position.string_of_position pos) ; find_new_pos g f (f p) balls

        and fun_of_dir d = match d with
          | Up -> fun p -> Position.from_int (Position.proj_x p) (Position.proj_y p + 1)
          | Down -> fun p -> Position.from_int (Position.proj_x p) (Position.proj_y p - 1)
          | Left -> fun p -> Position.from_int (Position.proj_x p - 1) (Position.proj_y p)
          | Right -> fun p -> Position.from_int (Position.proj_x p + 1) (Position.proj_y p)

      in find_new_pos g (fun_of_dir dir) p balls

    in get_ball_to_move g b dir (get_balls g) []
  else
  if first then g
  else new_game (remove_ball b (get_balls g) [])

  and remove_ball b balls acc = match balls with
    | [] -> acc
    | h::t -> if eq_ball h b then remove_ball b t acc else remove_ball b t (h::acc)

  in apply_move_aux g b dir true