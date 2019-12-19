open! Core
open! Async
open! Import

let check_point program (x, y) =
  let program = Intcode.run_program program in
  let input = Intcode.input program in
  input x;
  input y;
  match%bind Pipe.read (Intcode.output program) with
  | `Eof -> raise_s [%message "Unexpected EOF"]
  | `Ok 0 -> return false
  | `Ok 1 -> return true
  | `Ok n -> raise_s [%message "Unexpected output" (n : int)]
;;

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1

  let solve input =
    let l = List.range 0 50 in
    List.cartesian_product l l
    |> Deferred.List.map ~f:(check_point input)
    >>| List.count ~f:Fn.id
  ;;
end)

module Part_2 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 2

  let find_next check start direction =
    Deferred.repeat_until_finished start (fun location ->
        match%bind check location with
        | true -> return (`Finished location)
        | false -> return (`Repeat (Int_pair.add location direction)))
  ;;

  let solve input =
    let check_point = check_point input in
    let left_edge = Int_pair.Hash_set.create () in
    let right_edge = Int_pair.Hash_set.create () in
    let%bind x, y =
      Deferred.repeat_until_finished
        ((5, 4), (5, 4))
        (fun (left, right) ->
          let%bind next_left = find_next check_point (Int_pair.add left (0, 1)) (1, 0) in
          let%bind next_right =
            find_next check_point (Int_pair.add right (1, 0)) (0, 1)
          in
          Hash_set.add left_edge next_left;
          Hash_set.add right_edge next_right;
          match Hash_set.mem right_edge (Int_pair.add next_left (99, -99)) with
          | true -> return (`Finished (Int_pair.add next_left (0, -99)))
          | false ->
            (match Hash_set.mem left_edge (Int_pair.add next_right (-99, 99)) with
            | true -> return (`Finished (Int_pair.add next_right (-99, 0)))
            | false -> return (`Repeat (next_left, next_right))))
    in
    return ((x * 10_000) + y)
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 19
  let parts = [ Part_1.command; Part_2.command ]
end)
