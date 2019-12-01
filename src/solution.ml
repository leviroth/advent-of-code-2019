open! Core
open! Import
open Solution_intf

module Part = struct
  include Part_intf

  module Make (Solution : Part_intf.Basic) = struct
    include Solution

    let solve_file filename =
      Solution.Input.load filename |> Solution.solve |> Solution.Output.to_string
    ;;

    let solve_input input =
      Solution.Input.of_string input |> Solution.solve |> Solution.Output.to_string
    ;;

    let command ~day_of_month =
      let part_string = pad_int Solution.one_based_index in
      ( part_string
      , Command.basic
          ~summary:(sprintf "part %s solution" part_string)
          (Command.Param.return (fun () ->
               let input_file = sprintf "./input/day%02d.txt" day_of_month in
               solve_file input_file |> printf "%s\n")) )
    ;;

    module For_testing = struct
      let run = List.iter ~f:(Fn.compose print_endline solve_input)
    end
  end

  module Make_with_alternatives (Solution : Part_intf.Basic_with_alternatives) = struct
    include Make (Solution)

    module For_testing = struct
      let run =
        List.iter ~f:(fun input ->
            let input = Input.of_string input in
            let main_output = solve input in
            List.iter Solution.Alternatives.solutions ~f:(fun alternative ->
                let alternative_output = alternative input in
                match Output.equal main_output alternative_output with
                | true -> ()
                | false ->
                  raise_s
                    [%message
                      "Main and alternative solutions did not match"
                        (main_output : Output.t)
                        (alternative_output : Output.t)]);
            print_endline (Output.to_string main_output))
      ;;
    end
  end
end

module Day = struct
  include Day_intf

  module Make (Day : Day_intf.Basic) = struct
    include Day

    let command =
      let day_string = pad_int day_of_month in
      ( day_string
      , Command.group
          ~summary:(sprintf "day %s solutions" day_string)
          (List.map parts ~f:(fun (module Part : Part_intf.S) ->
               Part.command ~day_of_month)) )
    ;;
  end
end
