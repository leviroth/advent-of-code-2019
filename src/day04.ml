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

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1

  let criteria =
    let rec has_double list =
      match list with
      | [] | [ _ ] -> false
      | a :: (b :: _ as rest) ->
        (match Char.equal a b with
        | true -> true
        | false -> has_double rest)
    in
    let rec monotonically_increasing list =
      match list with
      | [] | [ _ ] -> true
      | a :: (b :: _ as rest) ->
        (match a <= b with
        | true -> monotonically_increasing rest
        | false -> false)
    in
    [ (fun n -> Int.to_string n |> String.to_list |> has_double)
    ; (fun n ->
        Int.to_string n
        |> String.to_list
        |> List.map ~f:String.of_char
        |> List.map ~f:Int.of_string
        |> monotonically_increasing)
    ]
  ;;

  let meets_criteria n = List.for_all criteria ~f:(fun criterion -> criterion n)
  let solve (lower, upper) = List.range lower upper |> List.count ~f:meets_criteria
end)

include Solution.Day.Make (struct
  let day_of_month = 4
  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
