open! Core
open! Async
open! Import

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1

  let solve input =
    let%bind scaffolds =
      let program = Intcode.run_program input in
      Pipe.to_list (Intcode.output program)
      >>| List.map ~f:Char.of_int_exn
      >>| String.of_char_list
      >>| String.split ~on:'\n'
      >>| List.map ~f:String.to_list
      >>| List.foldi ~init:Int_pair.Set.empty ~f:(fun y scaffolds ->
              List.foldi ~init:scaffolds ~f:(fun x scaffolds elem ->
                  match elem with
                  | '#' -> Set.add scaffolds (x, y)
                  | _ -> scaffolds))
    in
    let intersections =
      Set.filter scaffolds ~f:(fun scaffold ->
          List.for_all [ 1, 0; 0, 1; -1, 0; 0, -1 ] ~f:(fun shift ->
              Set.mem scaffolds (Int_pair.add scaffold shift)))
    in
    Set.to_list intersections |> List.sum (module Int) ~f:(Tuple2.uncurry ( * )) |> return
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 17
  let parts = [ Part_1.command ]
end)
