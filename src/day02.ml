open! Core
open! Async
open! Import

module Common = struct
  module Input = Intcode.Program
  module Output = Int

  let%expect_test "Run program without inputs" =
    let run_one_test input =
      let program = Intcode.Program.of_string input in
      let%bind state =
        Intcode.run_program program ~input:(Intcode.Input_port.of_list []) ~output:Intcode.Util.sink
      in
      print_s [%message (state : int list)];
      return ()
    in
    let%bind () =
      [ "1,0,0,0,99"; "2,3,0,3,99"; "2,4,4,5,99,0"; "1,1,1,4,99,5,6,0,99" ]
      |> Deferred.List.iter ~f:run_one_test
    in
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
    let%bind state =
      Intcode.run_program program ~input:(Intcode.Input_port.of_list []) ~output:Intcode.Util.sink
    in
    return (List.hd_exn state)
  ;;
end

include Solution.Day.Make (struct
  let day_of_month = 2

  module Part_1 = Solution.Part.Make_async (struct
    include Common

    let one_based_index = 1
    let solve program = run_program_with_inputs program 12 2
  end)

  module Part_2 = Solution.Part.Make_async (struct
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
      let%bind noun, verb =
        let%bind result =
          Deferred.Sequence.find zig_zag ~f:(fun (first, second) ->
              let%bind output = run_program_with_inputs program first second in
              return (output = target_output))
        in
        return (Option.value_exn result)
      in
      return ((noun * 100) + verb)
    ;;
  end)

  let parts = [ Part_1.command; Part_2.command ]
end)
