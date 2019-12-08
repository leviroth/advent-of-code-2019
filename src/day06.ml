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

let%expect_test _ =
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
  [%expect]
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve = Fn.compose count_orbits graph_of_input
end)

include Solution.Day.Make (struct
  let day_of_month = 6
  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
