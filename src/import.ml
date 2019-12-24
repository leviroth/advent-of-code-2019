open! Core

module Int_pair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)

  let add (a_1, b_1) (a_2, b_2) = a_1 + a_2, b_1 + b_2
  let sub (a_1, b_1) (a_2, b_2) = a_1 - a_2, b_1 - b_2
  let neighbors t directions = List.map directions ~f:(add t)
  let right_vectors = [ 1, 0; 0, 1; -1, 0; 0, -1 ]

  let print_set set =
    let range coordinates =
      let open Int in
      let max = List.max_elt coordinates ~compare |> Option.value_exn in
      let min = List.min_elt coordinates ~compare |> Option.value_exn in
      min, max
    in
    let far_left, far_right =
      let x_coordinates = Set.to_list set |> List.map ~f:fst in
      range x_coordinates
    in
    let bottom, top =
      let y_coordinates = Set.to_list set |> List.map ~f:snd in
      range y_coordinates
    in
    List.range ~stride:(-1) ~stop:`inclusive top bottom
    |> List.iter ~f:(fun y ->
           List.range ~stop:`inclusive far_left far_right
           |> List.iter ~f:(fun x ->
                  printf
                    "%c"
                    (match Set.mem set (x, y) with
                    | true -> '#'
                    | false -> ' '));
           printf "\n")
  ;;
end

let pad_int = sprintf "%02d"

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)
;;

let lcm a b = abs (a * b) / gcd a b
let option_value_exn x = Option.value_exn x
