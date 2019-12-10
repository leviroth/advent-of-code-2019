open! Core
open! Import

module Input = struct
  include String

  let load = Fn.compose String.strip In_channel.read_all
end

let width = 25
let height = 6

let layers input ~width ~height =
  String.to_list input |> List.groupi ~break:(fun i _ _ -> i mod (width * height) = 0)
;;

let layer_with_fewest_zeros layers =
  List.min_elt
    layers
    ~compare:(Comparable.lift compare ~f:(List.count ~f:(Char.equal '0')))
  |> Option.value_exn
;;

let checksum layer =
  List.count layer ~f:(Char.equal '1') * List.count layer ~f:(Char.equal '2')
;;

let align_layers input ~width ~height =
  let input = String.to_list input in
  let size = width * height in
  List.init size ~f:(fun bucket ->
      List.filteri input ~f:(fun i _elt -> i mod size = bucket))
;;

module Color = struct
  type t =
    | Black
    | White
    | Transparent

  let of_char char =
    match char with
    | '0' -> Black
    | '1' -> White
    | '2' -> Transparent
    | _ -> raise_s [%message "Unknown color char" (char : char)]
  ;;

  let to_print_char t =
    match t with
    | Black -> '#'
    | White -> ' '
    | Transparent -> '2'
  ;;
end

let color_of_pixel (pixel : Color.t list) =
  List.rev pixel
  |> List.fold ~init:Color.White ~f:(fun acc color ->
         match color with
         | Black -> Black
         | White -> White
         | Transparent -> acc)
;;

let solve_part_2 input ~width ~height =
  align_layers input ~width ~height
  |> List.map ~f:(List.map ~f:Color.of_char)
  |> List.map ~f:color_of_pixel
  |> List.map ~f:Color.to_print_char
  |> List.chunks_of ~length:width
  |> List.map ~f:String.of_char_list
  |> String.concat ~sep:"\n"
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve input = layers input ~width ~height |> layer_with_fewest_zeros |> checksum
end)

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = String

  let one_based_index = 2
  let solve = solve_part_2 ~width ~height
end)

let%expect_test "Part 2" =
  solve_part_2 "0222112222120000" ~width:2 ~height:2 |> print_endline;
  [%expect{|
    #
     # |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 8
  let parts = [ Part_1.command; Part_2.command ]
end)
