open! Core
open! Import
open Solution_intf

module Part = struct
  include Part_intf

  module Make (Solution : Synchronous.Basic) = struct
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
  end

  module Make_async (Solution : Asynchronous.Basic) = struct
    open! Async
    include Solution

    let solve_file filename =
      Solution.Input.load filename |> Solution.solve >>| Solution.Output.to_string
    ;;

    let solve_input input =
      Solution.Input.of_string input |> Solution.solve >>| Solution.Output.to_string
    ;;

    let command ~day_of_month =
      let part_string = pad_int Solution.one_based_index in
      ( part_string
      , Command.async
          ~summary:(sprintf "part %s solution" part_string)
          (Command.Param.return (fun () ->
               let input_file = sprintf "./input/day%02d.txt" day_of_month in
               solve_file input_file >>| printf "%s\n")) )
    ;;
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
          (List.map parts ~f:(fun command -> command ~day_of_month)) )
    ;;
  end
end
