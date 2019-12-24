open! Core
open! Import

module Square = struct
  type t =
    | Path
    | Wall
    | Portal of string
  [@@deriving sexp]

  module Raw = struct
    type t =
      | Path
      | Wall
      | Portal_segment of char
    [@@deriving sexp]

    let of_char c =
      match c with
      | '.' -> Path
      | '#' -> Wall
      | 'A' .. 'Z' -> Portal_segment c
      | _ -> raise_s [%message "Unknown square" (c : char)]
    ;;
  end
end

let portal_entrance (grid : Square.Raw.t Int_pair.Map.t) coordinates =
  List.find_map coordinates ~f:(fun coordinate ->
      Int_pair.neighbors coordinate Int_pair.right_vectors
      |> List.find_map ~f:(fun neighbor ->
             match Map.find grid neighbor with
             | Some Path -> Some (coordinate, neighbor)
             | _ -> None))
  |> Option.value_exn
;;

module Input = struct
  type t =
    { grid : Square.t Int_pair.Map.t
    ; portals : (Int_pair.t * Int_pair.t) list String.Map.t
    }
  [@@deriving sexp, fields]

  let raw_of_string string =
    let rows =
      String.split string ~on:'\n'
      |> List.filter ~f:(Fn.non String.is_empty)
      |> List.map ~f:String.to_list
    in
    List.concat_mapi rows ~f:(fun y row ->
        List.concat_mapi row ~f:(fun x element ->
            match element with
            | ' ' -> []
            | _ -> [ (x, y), Square.Raw.of_char element ]))
    |> Int_pair.Map.of_alist_exn
  ;;

  let of_raw raw_map =
    Map.fold
      raw_map
      ~init:{ grid = Int_pair.Map.empty; portals = String.Map.empty }
      ~f:(fun ~key:location ~data:raw t ->
        match raw with
        | Square.Raw.Path -> { t with grid = Map.set t.grid ~key:location ~data:Path }
        | Wall -> { t with grid = Map.set t.grid ~key:location ~data:Wall }
        | Portal_segment c ->
          (match
             Int_pair.neighbors location [ 1, 0; 0, 1 ]
             |> List.find_map ~f:(fun location' ->
                    let%bind.Option square = Map.find raw_map location' in
                    match square with
                    | Wall | Path -> None
                    | Portal_segment c' -> Some (location', c'))
           with
          | None -> t
          | Some (location', c') ->
            let portal_name = String.of_char_list [ c; c' ] in
            let portal_entrance = portal_entrance raw_map [ location; location' ] in
            { grid =
                [ location; location' ]
                |> List.fold ~init:t.grid ~f:(fun grid location ->
                       Map.set grid ~key:location ~data:(Portal portal_name))
            ; portals = Map.add_multi t.portals ~key:portal_name ~data:portal_entrance
            }))
  ;;

  let destination { grid; portals } location =
    match%bind.Option Map.find grid location with
    | Wall -> None
    | Path | Portal "ZZ" -> Some location
    | Portal portal_name ->
      Map.find_exn portals portal_name
      |> List.find_map ~f:(fun (entrance, exit) ->
             match Int_pair.equal location entrance with
             | true -> None
             | false -> Some exit)
  ;;

  let of_string = Fn.compose of_raw raw_of_string
  let load = Fn.compose of_string In_channel.read_all
end

let test_input =
  {|
         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z
|}
;;

let%expect_test "Parse" =
  let input = Input.of_string test_input in
  [%expect {| |}];
  let destination = Input.destination input (9, 7) in
  print_s [%message (destination : Int_pair.t option)];
  [%expect {| (destination ((2 8))) |}]
;;

let find_distance (t : Input.t) =
  let rec loop frontier next_frontier visited depth =
    match frontier with
    | [] -> loop next_frontier [] visited (depth + 1)
    | hd :: tl ->
      (match Map.find_exn t.grid hd with
      | Portal "ZZ" -> depth - 1
      | _ ->
        let neighbors =
          Int_pair.neighbors hd Int_pair.right_vectors
          |> List.filter ~f:(Fn.non (Set.mem visited))
          |> List.filter_map ~f:(Input.destination t)
        in
        loop
          tl
          (neighbors @ next_frontier)
          (List.fold neighbors ~init:visited ~f:Set.add)
          depth)
  in
  let start = Map.find_exn t.portals "AA" |> List.hd_exn |> snd in
  loop [ start ] [] Int_pair.Set.empty 0
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve = find_distance
end)

let%expect_test "Part 1" =
  Part_1.solve_input test_input |> print_endline;
  [%expect {| 23 |}]
;;

let%expect_test _ =
  let input =
    {|
                   A
                   A
  #################.#############
  #.#...#...................#.#.#
  #.#.#.###.###.###.#########.#.#
  #.#.#.......#...#.....#.#.#...#
  #.#########.###.#####.#.#.###.#
  #.............#.#.....#.......#
  ###.###########.###.#####.#.#.#
  #.....#        A   C    #.#.#.#
  #######        S   P    #####.#
  #.#...#                 #......VT
  #.#.#.#                 #.#####
  #...#.#               YN....#.#
  #.###.#                 #####.#
DI....#.#                 #.....#
  #####.#                 #.###.#
ZZ......#               QG....#..AS
  ###.###                 #######
JO..#.#.#                 #.....#
  #.#.#.#                 ###.#.#
  #...#..DI             BU....#..LF
  #####.#                 #.#####
YN......#               VT..#....QG
  #.###.#                 #.###.#
  #.#...#                 #.....#
  ###.###    J L     J    #.#.###
  #.....#    O F     P    #.#...#
  #.###.#####.#.#####.#####.###.#
  #...#.#.#...#.....#.....#.#...#
  #.#####.###.###.#.#.#########.#
  #...#.#.....#...#.#.#.#.....#.#
  #.###.#####.###.###.#.#.#######
  #.#.........#...#.............#
  #########.###.###.#############
           B   J   C
           U   P   P
|}
  in
  Part_1.solve_input input |> print_endline;
  [%expect {| 58 |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 20
  let parts = [ Part_1.command ]
end)
