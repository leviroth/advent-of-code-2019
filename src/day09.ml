open! Core
open! Async
open! Import

let%expect_test "Part 1" =
  let programs =
    [ [ 109; 1; 204; -1; 1001; 100; 1; 100; 1008; 100; 16; 101; 1006; 101; 0; 99 ]
    ; [ 1102; 34915192; 34915192; 7; 4; 7; 99; 0 ]
    ; [ 104; 1125899906842624; 99 ]
    ]
  in
  let%bind () =
    Deferred.List.iter programs ~f:(fun program ->
        let program = Intcode.run_program program in
        let%bind output = Pipe.to_list (Intcode.output program) in
        print_s [%message (output : int list)];
        return ())
  in
  [%expect
    {|
    (output (109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))
    (output (1219070632396864))
    (output (1125899906842624)) |}]
;;

let solve program input =
  let program = Intcode.run_program program in
  Pipe.write_without_pushback (Intcode.input program) input;
  Pipe.to_list (Intcode.output program)
;;

include Solution.Day.Make (struct
  let day_of_month = 9

  module Common = struct
    module Input = Intcode.Program

    module Output = struct
      type t = int list

      let to_string t = [%sexp_of: int list] t |> Sexp.to_string
    end
  end

  module Part_1 = Solution.Part.Make_async (struct
    include Common

    let one_based_index = 1
    let solve program = solve program 1
  end)

  module Part_2 = Solution.Part.Make_async (struct
    include Common

    let one_based_index = 2
    let solve program = solve program 2
  end)

  let parts = [ Part_1.command; Part_2.command ]
end)
