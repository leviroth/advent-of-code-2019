open! Core
open! Import

let%expect_test "Part 2" =
  let program =
    {|
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
|}
    |> Intcode.Input.of_string
  in
  let test_cases = [ 3; 7; 8; 9; 100 ] in
  List.iter test_cases ~f:(fun input ->
      print_s [%message (input : int)];
      let ({ output; _ } : Intcode.Result.t) =
        Intcode.run_program program ~input:[ input ]
      in
      print_s [%message (output : int list)]);
  [%expect
    {|
    (input 3)
    (output (999))
    (input 7)
    (output (999))
    (input 8)
    (output (1000))
    (input 9)
    (output (1001))
    (input 100)
    (output (1001)) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 5

  module Part_1 = Solution.Part.Make (struct
    module Input = Intcode.Input
    module Output = Int

    let one_based_index = 1

    let solve program =
      let ({ output; _ } : Intcode.Result.t) = Intcode.run_program program ~input:[ 1 ] in
      List.last_exn output
    ;;
  end)

  module Part_2 = Solution.Part.Make (struct
    module Input = Intcode.Input
    module Output = Int

    let one_based_index = 2

    let solve program =
      let ({ output; _ } : Intcode.Result.t) = Intcode.run_program program ~input:[ 5 ] in
      List.last_exn output
    ;;
  end)

  let parts : (module Solution.Part.S) list = [ (module Part_1); (module Part_2) ]
end)
