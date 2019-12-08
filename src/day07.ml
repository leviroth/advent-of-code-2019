open! Core
open! Import

let run_one program phase input_signal =
  let ({ output; _ } : Intcode.Result.t) =
    Intcode.run_program program ~input:[ phase; input_signal ]
  in
  List.hd_exn output
;;

let run_sequence program sequence =
  List.fold sequence ~init:0 ~f:(fun input_signal phase ->
      run_one program phase input_signal)
;;

let%expect_test "Part 1" =
  let inputs =
    [ "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", [ 4; 3; 2; 1; 0 ]
    ; ( "3,23,3,24,1002,24,10,24,1002,23,-1,23,\n101,5,23,23,1,24,23,23,4,23,99,0,0"
      , [ 0; 1; 2; 3; 4 ] )
    ; ( "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\n\
         1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
      , [ 1; 0; 4; 3; 2 ] )
    ]
  in
  List.iter inputs ~f:(fun (program, sequence) ->
      let program = Intcode.Input.of_string program in
      let output = run_sequence program sequence in
      print_s [%message (output : int)]);
  [%expect {|
    (output 43210)
    (output 54321)
    (output 65210) |}]
;;

let sequences =
  let rec permutations list =
    match list with
    | [] -> [ [] ]
    | _ ->
      List.concat_map list ~f:(fun elt ->
          List.filter list ~f:(Fn.non (Int.equal elt))
          |> permutations
          |> List.map ~f:(fun permutation -> elt :: permutation))
  in
  List.range 0 5 |> permutations
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Intcode.Input
  module Output = Int

  let one_based_index = 1

  let solve program =
    List.map sequences ~f:(run_sequence program)
    |> List.max_elt ~compare
    |> Option.value_exn
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 7
  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
