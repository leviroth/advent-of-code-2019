open! Core
open! Import

include Solution.Day.Make (struct
  let day_of_month = 1

  module Common = struct
    module Input = Converters.Int_list
    module Output = Int

    let fuel_requirement n = (n / 3) - 2
    let test_cases = [ "12"; "14"; "1969"; "100756" ]
  end

  module Part_1 = Solution.Part.Make (struct
    include Common

    let one_based_index = 1
    let solve = List.sum (module Int) ~f:fuel_requirement
  end)

  let%expect_test "Part 1" =
    Part_1.For_testing.run ();
    [%expect {|
      2
      2
      654
      33583 |}]
  ;;

  module Part_2 = Solution.Part.Make_with_alternatives (struct
    include Common

    let one_based_index = 2

    let fuel_requirement_accounting_for_extra n =
      let rec aux n total =
        let required_fuel = fuel_requirement n in
        match required_fuel <= 0 with
        | true -> total
        | false -> aux required_fuel (total + required_fuel)
      in
      aux n 0
    ;;

    let solve = List.sum (module Int) ~f:fuel_requirement_accounting_for_extra

    let alternatives =
      [ (let rec fuel_requirement_accounting_for_extra n =
           match max 0 (Common.fuel_requirement n) with
           | 0 -> 0
           | required_fuel ->
             required_fuel + fuel_requirement_accounting_for_extra required_fuel
         in
         List.sum (module Int) ~f:fuel_requirement_accounting_for_extra)
      ]
    ;;
  end)

  let%expect_test "Part 2" =
    Part_2.For_testing.run ();
    [%expect {|
      2
      2
      966
      50346 |}]
  ;;

  let parts : (module Solution.Part.S) list = [ (module Part_1); (module Part_2) ]
end)
