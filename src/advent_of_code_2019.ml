open! Core
open! Import

let days : (module Solution.Day.S) list = [ (module Day01) ]

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.map days ~f:(fun (module Day) -> Day.command))
;;
