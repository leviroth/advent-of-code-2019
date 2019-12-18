open! Core
open! Import

module Square = struct
  type t =
    | Path
    | Wall
    | Key of char
    | Door of char
    | Start
  [@@deriving equal]

  let of_char c =
    match c with
    | '.' -> Path
    | '#' -> Wall
    | '@' -> Start
    | _ ->
      (match Char.is_uppercase c with
      | true -> Door c
      | false ->
        (match Char.is_lowercase c with
        | true -> Key (Char.uppercase c)
        | false -> raise_s [%message "Unknown character" (c : char)]))
  ;;
end

let get_grid string =
  String.split string ~on:'\n'
  |> List.map ~f:String.to_list
  |> List.foldi ~init:Int_pair.Map.empty ~f:(fun y grid ->
         List.foldi ~init:grid ~f:(fun x grid elem ->
             Map.add_exn grid ~key:(x, y) ~data:(Square.of_char elem)))
;;

module Input = struct
  type t = Square.t Int_pair.Map.t

  let of_string = get_grid
  let load filename = In_channel.read_all filename |> String.strip |> of_string
end

let starting_squares (grid : Input.t) =
  Map.to_alist grid
  |> List.filter_map ~f:(function location, square ->
         (match square with
         | Start -> Some location
         | _ -> None))
;;

let keys (grid : Input.t) =
  Map.filter_map grid ~f:(function
      | Key c -> Some c
      | _ -> None)
;;

let locations_by_key grid =
  keys grid |> Map.to_alist |> List.map ~f:Tuple2.swap |> Char.Map.of_alist_exn
;;

let right_vectors = [ 1, 0; 0, 1; -1, 0; 0, -1 ]

let diagonal_vectors =
  let l = [ 1; -1 ] in
  List.cartesian_product l l
;;

let neighbors (grid : Input.t) location =
  List.map right_vectors ~f:(Int_pair.add location)
  |> List.filter ~f:(function location ->
         (match Map.find_exn grid location with
         | Wall -> false
         | _ -> true))
;;

let distance (grid : Input.t) start =
  let path_info visited endpoint =
    let rec aux current length doors =
      match Map.find visited current |> Option.join with
      | None -> length, doors
      | Some parent ->
        aux
          parent
          (length + 1)
          (match Map.find_exn grid parent with
          | Door door -> Set.add doors door
          | _ -> doors)
    in
    aux endpoint 0 Char.Set.empty
  in
  let rec loop frontier visited results =
    let (new_visited, new_results), new_frontier =
      List.fold_map
        frontier
        ~init:(visited, results)
        ~f:(fun (visited, results) location ->
          let neighbors =
            neighbors grid location |> List.filter ~f:(Fn.non (Map.mem visited))
          in
          let new_visited =
            List.fold neighbors ~init:visited ~f:(fun map neighbor ->
                Map.add_exn map ~key:neighbor ~data:(Some location))
          in
          match Map.find_exn grid location with
          | Key _ ->
            ( ( new_visited
              , Map.add_exn results ~key:location ~data:(path_info visited location) )
            , neighbors )
          | _ -> (new_visited, results), neighbors)
      |> Tuple2.map_snd ~f:List.concat
    in
    match List.is_empty frontier with
    | true -> new_results
    | false -> loop new_frontier new_visited new_results
  in
  loop [ start ] (Int_pair.Map.singleton start None) Int_pair.Map.empty
;;

let%expect_test _ =
  let grid = Input.of_string {|#########
#b.A.@.a#
#########|} in
  let starting_square = starting_squares grid |> List.hd_exn in
  distance grid starting_square
  |> [%sexp_of: (int * Char.Set.t) Int_pair.Map.t]
  |> print_s;
  [%expect {| (((1 1) (4 (A))) ((7 1) (2 ()))) |}]
;;

let all_distances grid =
  let locations_by_key = locations_by_key grid in
  let alist : (Int_pair.t * (int * Char.Set.t) Int_pair.Map.t) list =
    (starting_squares grid
    |> List.map ~f:(fun starting_square -> starting_square, distance grid starting_square)
    )
    @ (Map.data locations_by_key |> List.map ~f:(fun key -> key, distance grid key))
  in
  Int_pair.Map.of_alist_exn alist
;;

module State = struct
  module T = struct
    type t =
      { current : Int_pair.t list
      ; collected_keys : char list
      }
    [@@deriving hash, sexp, compare]
  end

  include T
  include Hashable.Make (T)

  let uncollected_keys { collected_keys; _ } ~all_keys =
    List.fold collected_keys ~init:all_keys ~f:Set.remove
  ;;
end

let update_list l idx value = List.take l idx @ [ value ] @ List.drop l (idx + 1)

let solve grid start =
  let key_distances = all_distances grid in
  let locations_by_key = locations_by_key grid in
  let all_keys = locations_by_key |> Map.key_set in
  let costs = State.Table.create () in
  let rec cost_of_uncollected_keys ({ current; collected_keys } as state : State.t) =
    match Hashtbl.find costs state with
    | Some cost -> cost
    | None ->
      let uncollected_keys = State.uncollected_keys state ~all_keys in
      (match Set.is_empty uncollected_keys with
      | true -> 0
      | false ->
        let reachable_now =
          let doors target =
            let target_location = Map.find_exn locations_by_key target in
            List.find_map current ~f:(fun robot ->
                let distances_from_robot = Map.find_exn key_distances robot in
                Map.find distances_from_robot target_location)
            |> Option.value_exn
            |> snd
            |> Set.to_list
          in
          Set.filter uncollected_keys ~f:(fun key ->
              List.for_all (doors key) ~f:(fun door ->
                  not (Set.mem uncollected_keys door)))
          |> Set.to_list
        in
        let best =
          List.map reachable_now ~f:(fun target ->
              let location = Map.find_exn locations_by_key target in
              let robot_to_move, distance =
                List.find_mapi current ~f:(fun i robot ->
                    let distances_from_robot = Map.find_exn key_distances robot in
                    Map.find distances_from_robot location
                    |> Option.map ~f:(fun distance -> i, fst distance))
                |> Option.value_exn
              in
              let new_state : State.t =
                { current = update_list current robot_to_move location
                ; collected_keys =
                    List.merge ~compare:Char.compare [ target ] collected_keys
                }
              in
              cost_of_uncollected_keys new_state + distance)
          |> List.min_elt ~compare
          |> Option.value_exn
        in
        Hashtbl.set costs ~key:state ~data:best;
        best)
  in
  cost_of_uncollected_keys { current = start; collected_keys = [] }
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1

  let solve input =
    let start = starting_squares input in
    solve input start
  ;;
end)

let%expect_test "Part 1" =
  let grid =
    {|#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################|}
  in
  Part_1.solve_input grid |> print_endline;
  [%expect {| 136 |}]
;;

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 2

  let solve input =
    let start = starting_squares input |> List.hd_exn in
    let to_walls = start :: List.map right_vectors ~f:(Int_pair.add start) in
    let starts = List.map diagonal_vectors ~f:(Int_pair.add start) in
    let grid =
      List.fold starts ~init:input ~f:(fun grid key -> Map.set grid ~key ~data:Start)
    in
    let grid =
      List.fold to_walls ~init:grid ~f:(fun grid key -> Map.set grid ~key ~data:Wall)
    in
    solve grid starts
  ;;
end)

let%expect_test "Part 2" =
  let grid =
    {|#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############|}
  in
  Part_2.solve_input grid |> print_endline;
  [%expect {| 32 |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 18
  let parts = [ Part_1.command; Part_2.command ]
end)
