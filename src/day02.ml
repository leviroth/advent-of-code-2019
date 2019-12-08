open! Core
open! Import

module Common = struct
  module Input = Intcode.Input
  module Output = Int

  let%expect_test "Run program without inputs" =
    let run_one_test input =
      let program = Intcode.Input.of_string input in
      let ({ state; _ } : Intcode.Result.t) = Intcode.run_program program ~input:[] in
      print_s [%message (state : int list)]
    in
    [ "1,0,0,0,99"; "2,3,0,3,99"; "2,4,4,5,99,0"; "1,1,1,4,99,5,6,0,99" ]
    |> List.iter ~f:run_one_test;
    [%expect
      {|
        (state (2 0 0 0 99))
        (state (2 3 0 6 99))
        (state (2 4 4 5 99 9801))
        (state (30 1 1 4 2 5 6 0 99)) |}]
  ;;

  let run_program_with_inputs program first second =
    let program = Array.of_list program in
    program.(1) <- first;
    program.(2) <- second;
    let program = Array.to_list program in
    let ({ state; _ } : Intcode.Result.t) = Intcode.run_program program ~input:[] in
    List.hd_exn state
  ;;
end

include Solution.Day.Make (struct
  let day_of_month = 2

  module Part_1 = Solution.Part.Make (struct
    include Common

    let one_based_index = 1
    let solve program = run_program_with_inputs program 12 2
  end)

  module Part_2 = Solution.Part.Make (struct
    include Common

    let one_based_index = 2
    let target_output = 19690720

    let solve program =
      let zig_zag =
        Sequence.unfold_step ~init:(0, 0) ~f:(fun (first, second) ->
            match first <= second with
            | true -> Skip (first + 1, 0)
            | false -> Yield ((first, second), (first, second + 1)))
      in
      let noun, verb =
        Sequence.find zig_zag ~f:(fun (first, second) ->
            run_program_with_inputs program first second = target_output)
        |> Option.value_exn
      in
      (noun * 100) + verb
    ;;
  end)

  let parts : (module Solution.Part.S) list = [ (module Part_1); (module Part_2) ]
end)
