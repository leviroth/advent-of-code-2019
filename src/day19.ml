open! Core
open! Async
open! Import

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1

  let solve input =
    let l = List.range 0 50 in
    List.cartesian_product l l
    |> Deferred.List.map ~f:(fun (x, y) ->
           let program = Intcode.run_program input in
           let input = Intcode.input program in
           input x;
           input y;
           match%bind Pipe.read (Intcode.output program) with
           | `Eof -> raise_s [%message "Unexpected EOF"]
           | `Ok 0 -> return false
           | `Ok 1 -> return true
           | `Ok n -> raise_s [%message "Unexpected output" (n : int)])
    >>| List.count ~f:Fn.id
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 19
  let parts = [ Part_1.command ]
end)
