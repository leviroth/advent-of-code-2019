open! Core
open! Async
open! Import

let%expect_test "Part 2" =
  let program =
    {|
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
|}
    |> Intcode.Program.of_string
  in
  let test_cases = [ 3; 7; 8; 9; 100 ] in
  let%bind () =
    Deferred.List.iter test_cases ~f:(fun input ->
        print_s [%message (input : int)];
        let program = Intcode.run_program program in
        Pipe.write_without_pushback (Intcode.input program) input;
        let%bind output = Pipe.to_list (Intcode.output program) in
        print_s [%message (output : int list)];
        return ())
  in
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

let solve program input =
  let program = Intcode.run_program program in
  Pipe.write_without_pushback (Intcode.input program) input;
  let%bind output = Pipe.to_list (Intcode.output program) in
  return (List.last_exn output)
;;

include Solution.Day.Make (struct
  let day_of_month = 5

  module Part_1 = Solution.Part.Make_async (struct
    module Input = Intcode.Program
    module Output = Int

    let one_based_index = 1
    let solve program = solve program 1
  end)

  module Part_2 = Solution.Part.Make_async (struct
    module Input = Intcode.Program
    module Output = Int

    let one_based_index = 2
    let solve program = solve program 5
  end)

  let parts = [ Part_1.command; Part_2.command ]
end)
