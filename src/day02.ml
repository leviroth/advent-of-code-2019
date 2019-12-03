open! Core
open! Import

module Common = struct
  module Input = Input.Make_parseable (struct
    type t = int list

    let parser =
      let open Angstrom in
      let integer =
        take_while1 (function
            | '0' .. '9' -> true
            | _ -> false)
        >>| int_of_string
      in
      sep_by (char ',') integer
    ;;
  end)

  let%expect_test "Parser" =
    Input.of_string "1,0,0,0,99" |> [%sexp_of: int list] |> print_s;
    [%expect {| (1 0 0 0 99) |}]
  ;;

  module Output = Int

  let run_program program =
    let get_indirect index = program.(program.(index)) in
    let set_indirect index value = program.(program.(index)) <- value in
    let rec solve index =
      let apply_simple_op operator =
        let value = operator (get_indirect (index + 1)) (get_indirect (index + 2)) in
        set_indirect (index + 3) value;
        solve (index + 4)
      in
      match program.(index) with
      | 1 -> apply_simple_op ( + )
      | 2 -> apply_simple_op ( * )
      | 99 -> ()
      | opcode -> raise_s [%message "Unexpected opcode" (index : int) (opcode : int)]
    in
    solve 0
  ;;

  let%expect_test "Run program without inputs" =
    let run_one_test input =
      let program = Array.of_list (Input.of_string input) in
      run_program program;
      print_s [%message (program : int array)]
    in
    [ "1,0,0,0,99"; "2,3,0,3,99"; "2,4,4,5,99,0"; "1,1,1,4,99,5,6,0,99" ]
    |> List.iter ~f:run_one_test;
    [%expect
      {|
        (program (2 0 0 0 99))
        (program (2 3 0 6 99))
        (program (2 4 4 5 99 9801))
        (program (30 1 1 4 2 5 6 0 99)) |}]
  ;;

  let run_program_with_inputs program first second =
    let program = Array.of_list program in
    program.(1) <- first;
    program.(2) <- second;
    run_program program;
    program.(0)
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
