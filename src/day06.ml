open! Core
open! Import

module Orbit = struct
  type t =
    { center : string
    ; orbiter : string
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    lift2
      (fun center orbiter -> { center; orbiter })
      (skip_while Char.is_whitespace *> take_while1 (Char.( <> ) ')') <* char ')')
      (take_while1 (Fn.non Char.is_whitespace))
  ;;
end

module Input = struct
  type t = Orbit.t list

  include Input.Make_parseable_many (Orbit)
end

let graph_of_input (input : Input.t) =
  List.map input ~f:(fun { center; orbiter } -> center, orbiter)
  |> String.Map.of_alist_multi
;;

let count_orbits graph =
  let rec aux planet depth =
    match Map.find graph planet with
    | None -> depth
    | Some orbiters ->
      depth + List.sum (module Int) orbiters ~f:(fun planet -> aux planet (depth + 1))
  in
  aux "COM" 0
;;

let%expect_test "Part 1" =
  let graph =
    {|
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
|}
    |> Input.of_string
    |> graph_of_input
  in
  print_s [%message (graph : string list String.Map.t)];
  let count = count_orbits graph in
  print_s [%message (count : int)];
  [%expect{|
    (graph
     ((B (C G)) (C (D)) (COM (B)) (D (E I)) (E (F J)) (G (H)) (J (K)) (K (L))))
    (count 42) |}]
;;

let undirected_graph_of_input (input : Input.t) =
  List.concat_map input ~f:(fun { center; orbiter } ->
      [ center, orbiter; orbiter, center ])
  |> String.Map.of_alist_multi
;;

let find_distance graph start stop =
  let rec loop frontier next_frontier visited depth =
    match frontier with
    | [] -> loop next_frontier [] visited (depth + 1)
    | hd :: tl ->
      (match String.equal hd stop with
      | true -> depth
      | false ->
        let neighbors =
          Map.find_exn graph hd |> List.filter ~f:(Fn.non (Set.mem visited))
        in
        loop
          tl
          (neighbors @ next_frontier)
          (List.fold neighbors ~init:visited ~f:Set.add)
          depth)
  in
  loop [ start ] [] String.Set.empty 0 - 2
;;

let%expect_test "Part 2" =
  let graph =
    {|
      COM)B
      B)C
      C)D
      D)E
      E)F
      B)G
      G)H
      D)I
      E)J
      J)K
      K)L
      K)YOU
      I)SAN
    |}
    |> Input.of_string
    |> undirected_graph_of_input
  in
  let count = find_distance graph "YOU" "SAN" in
  print_s [%message (count : int)];
  [%expect{| (count 4) |}]
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve = Fn.compose count_orbits graph_of_input
end)

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 2

  let solve input =
    let graph = undirected_graph_of_input input in
    find_distance graph "YOU" "SAN"
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 6
  let parts : (module Solution.Part.S) list = [ (module Part_1); (module Part_2) ]
end)
