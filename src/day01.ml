open! Core
open! Import

include Solution.Day.Make (struct
  let day_of_month = 1

  module Common = struct
    module Input = Converters.Int_list
    module Output = Int
  end

  module Part_1 = Solution.Part.Make (struct
    include Common

    let one_based_index = 1
    let fuel_requirement n = (n / 3) - 2
    let solve = List.sum (module Int) ~f:fuel_requirement
  end)

  let%expect_test "Part 1" =
    let part_1_test_cases = [ "12"; "14"; "1969"; "100756" ] in
    List.iter part_1_test_cases ~f:(Fn.compose print_endline Part_1.solve_input);
    [%expect {|
    2
    2
    654
    33583 |}]
  ;;

  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
