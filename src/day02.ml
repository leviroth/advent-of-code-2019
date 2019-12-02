open! Core
open! Import

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

include Solution.Day.Make (struct
  let day_of_month = 2

  module Part_1 = Solution.Part.Make (struct
    module Input = Input
    module Output = Int

    let one_based_index = 1

    let run_program program =
      let rec solve index =
        let apply_simple_op operator =
          let value =
            operator program.(program.(index + 1)) program.(program.(index + 2))
          in
          program.(program.(index + 3)) <- value;
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

    let%expect_test "Part 1" =
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

    let solve program =
      let program = Array.of_list program in
      program.(1) <- 12;
      program.(2) <- 2;
      run_program program;
      program.(0)
    ;;
  end)

  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
