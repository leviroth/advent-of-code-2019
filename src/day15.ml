open! Core
open! Async
open! Import

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, enumerate]

  let opposite t =
    match t with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left
  ;;

  let to_vector t =
    match t with
    | Up -> 0, 1
    | Down -> 0, -1
    | Left -> -1, 0
    | Right -> 1, 0
  ;;

  let to_int_code t =
    match t with
    | Up -> 1
    | Down -> 2
    | Left -> 3
    | Right -> 4
  ;;
end

let go_in_direction program direction =
  let input = Intcode.input program in
  input (Direction.to_int_code direction);
  let output = Intcode.output program in
  match%bind Pipe.read output with
  | `Eof -> raise_s [%message "Unexpected EOF"]
  | `Ok 0 -> return `Blocked
  | `Ok 1 -> return `Moved
  | `Ok 2 -> return `Finished
  | `Ok n -> raise_s [%message "Unexpected move response" (n : int)]
;;

let find_target program =
  let run_from_origin max_depth =
    let program = Intcode.run_program program in
    let visited = Int_pair.Hash_set.create () in
    let rec loop position depth =
      match Hash_set.mem visited position || depth > max_depth with
      | true -> return None
      | false ->
        Hash_set.add visited position;
        Deferred.List.find_map Direction.all ~f:(fun direction ->
            match%bind go_in_direction program direction with
            | `Blocked -> return None
            | `Finished -> return (Some (depth + 1))
            | `Moved ->
              (match%bind
                 loop (Int_pair.add position (Direction.to_vector direction)) (depth + 1)
               with
              | None ->
                let%bind _ = go_in_direction program (Direction.opposite direction) in
                return None
              | Some answer -> return (Some answer)))
    in
    loop (0, 0) 0
  in
  Deferred.repeat_until_finished 1 (fun max_depth ->
      match%bind run_from_origin max_depth with
      | None -> return (`Repeat (max_depth + 1))
      | Some answer -> return (`Finished answer))
;;

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1
  let solve = find_target
end)

let find_empty_space program =
  let oxygen_source = ref None in
  let run_from_origin max_depth =
    let program = Intcode.run_program program in
    let visited = Int_pair.Hash_set.create () in
    let rec loop position depth =
      match Hash_set.mem visited position || depth > max_depth with
      | true -> return ()
      | false ->
        Hash_set.add visited position;
        Deferred.List.iter Direction.all ~f:(fun direction ->
            match%bind go_in_direction program direction with
            | `Blocked -> return ()
            | `Finished ->
              let new_position = Int_pair.add position (Direction.to_vector direction) in
              oxygen_source := Some new_position;
              let%bind () = loop new_position (depth + 1) in
              let%bind _ = go_in_direction program (Direction.opposite direction) in
              return ()
            | `Moved ->
              let%bind () =
                loop (Int_pair.add position (Direction.to_vector direction)) (depth + 1)
              in
              let%bind _ = go_in_direction program (Direction.opposite direction) in
              return ())
    in
    let%bind () = loop (0, 0) 0 in
    return (Hash_set.to_list visited)
  in
  let%bind all_to_walk =
    Deferred.repeat_until_finished (1, 0) (fun (max_depth, size_of_previous_visited) ->
        let%bind visited = run_from_origin max_depth in
        match size_of_previous_visited = List.length visited with
        | true -> return (`Finished (Int_pair.Set.of_list visited))
        | false -> return (`Repeat (max_depth + 1, List.length visited)))
  in
  let rec bfs depth frontier visited =
    let to_visit =
      List.concat_map frontier ~f:(fun position ->
          List.map Direction.all ~f:(fun direction ->
              Int_pair.add position (Direction.to_vector direction)))
      |> List.filter ~f:(Set.mem all_to_walk)
      |> List.filter ~f:(Fn.non (Set.mem visited))
    in
    match List.is_empty to_visit with
    | true -> depth
    | false ->
      let visited = List.fold to_visit ~init:visited ~f:Set.add in
      bfs (depth + 1) to_visit visited
  in
  let oxygen_source = Option.value_exn !oxygen_source in
  bfs 0 [ oxygen_source ] (Int_pair.Set.singleton oxygen_source) |> return
;;

let%expect_test _ =
  let all_to_walk = Int_pair.Set.of_list [ 0, 0; 1, 0; 1, 1; 0, 1 ] in
  let rec bfs depth frontier visited =
    let to_visit =
      List.concat_map frontier ~f:(fun position ->
          List.map Direction.all ~f:(fun direction ->
              Int_pair.add position (Direction.to_vector direction)))
      |> List.filter ~f:(Set.mem all_to_walk)
      |> List.filter ~f:(Fn.non (Set.mem visited))
    in
    match List.is_empty to_visit with
    | true -> depth
    | false ->
      let visited = List.fold to_visit ~init:visited ~f:Set.add in
      bfs (depth + 1) to_visit visited
  in
  print_int (bfs 0 [ 0, 0 ] (Int_pair.Set.singleton (0, 0)));
  [%expect {| 2 |}]
;;

module Part_2 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 2
  let solve = find_empty_space
end)

let%expect_test _ =
  (* Test case courtesy of /u/bjnord on Reddit: https://redd.it/eb9ms2 *)
  let input =
    {|
3,70,9,70,2001,71,74,73,2001,72,78,74,1002,70,-1,70,9,70,2,73,100,83,1,83,74,83,9,83,1201,116,0,84,1002,83,-1,83,9,83,1006,84,49,1001,73,0,71,1001,74,0,72,4,84,1105,1,0,99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,1,0,0,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,-1,0,0,-1,-1,-1,0,1,1,0,0,-1,0,1,0,1,1,0,0,1,2,1,0,-1,-1,0,0,0,-1,-1
|}
  in
  let%bind () = Part_2.solve_input input >>| print_endline in
  [%expect {| 4 |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 15
  let parts = [ Part_1.command; Part_2.command ]
end)
