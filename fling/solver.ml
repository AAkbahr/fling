let solve game =
  let rec dfs game moves acc =
    print_string "DFS\n";
    Rules.print_moves acc;
    match (Rules.get_balls game) with
    | [] -> failwith "No ball!"
    | [x] -> Some(acc)
    | h::t -> match moves with
      | [] -> None
      | [m] -> let g = Rules.apply_move game m in
        dfs g (Rules.moves g) (m::acc)
      | mh::mt -> let g = Rules.apply_move game mh in
        let path = dfs g (Rules.moves g) (mh::acc) in
        if path = None then dfs game mt acc else path
  in dfs game (Rules.moves game) []
