open! Core
open! Import

module Input = Input.Make_parseable (struct
  type t = int * int [@@deriving sexp]

  let parser =
    let open Angstrom in
    let integer =
      take_while1 (function
          | '0' .. '9' -> true
          | _ -> false)
      >>| int_of_string
    in
    lift2 Tuple2.create (integer <* char '-') integer
  ;;
end)

let meets_criteria criteria n = List.for_all criteria ~f:(fun criterion -> criterion n)

let solve_criteria criteria (lower, upper) =
  List.range lower upper |> List.count ~f:(meets_criteria criteria)
;;

let to_char_list n = Int.to_string n |> String.to_list

let to_int_list n =
  to_char_list n |> List.map ~f:String.of_char |> List.map ~f:Int.of_string
;;

let has_double n =
  let rec loop list =
    match list with
    | [] | [ _ ] -> false
    | a :: (b :: _ as rest) ->
      (match Char.equal a b with
      | true -> true
      | false -> loop rest)
  in
  to_char_list n |> loop
;;

let run_tests cases criterion =
  List.iter cases ~f:(fun n ->
      print_s [%message (n : int) ~meets_criterion:(criterion n : bool)])
;;

let part_1_cases = [ 111111; 223450; 123789 ]

let%expect_test "has_double" =
  run_tests part_1_cases has_double;
  [%expect
    {|
    ((n 111111) (meets_criterion true))
    ((n 223450) (meets_criterion true))
    ((n 123789) (meets_criterion false)) |}]
;;

let monotonically_increasing n =
  let rec loop list =
    match list with
    | [] | [ _ ] -> true
    | a :: (b :: _ as rest) ->
      (match a <= b with
      | false -> false
      | true -> loop rest)
  in
  to_int_list n |> loop
;;

let%expect_test "monotonically_increasing" =
  run_tests part_1_cases monotonically_increasing;
  [%expect
    {|
    ((n 111111) (meets_criterion true))
    ((n 223450) (meets_criterion false))
    ((n 123789) (meets_criterion true)) |}]
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve = solve_criteria [ has_double; monotonically_increasing ]
end)

let has_strict_double n =
  let rec loop list run =
    match list with
    | [] ->
      (match run with
      | Some (_, 2) -> true
      | _ -> false)
    | hd :: tl ->
      (match run with
      | None -> loop tl (Some (hd, 1))
      | Some (current_element, count) ->
        (match count, Char.equal current_element hd with
        | 2, false -> true
        | 2, true -> loop tl (Some (hd, count + 1))
        | _, false -> loop tl (Some (hd, 1))
        | _, true -> loop tl (Some (hd, count + 1))))
  in
  let list = to_char_list n in
  loop list None
;;

let part_2_cases = [ 112233; 123444; 111122 ]

let%expect_test "has_strict_double" =
  run_tests part_2_cases has_strict_double;
  [%expect
    {|
    ((n 112233) (meets_criterion true))
    ((n 123444) (meets_criterion false))
    ((n 111122) (meets_criterion true)) |}]
;;

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 2
  let solve = solve_criteria [ has_strict_double; monotonically_increasing ]
end)

include Solution.Day.Make (struct
  let day_of_month = 4
  let parts : (module Solution.Part.S) list = [ (module Part_1); (module Part_2) ]
end)
