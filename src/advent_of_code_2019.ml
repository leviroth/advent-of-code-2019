open! Core
open! Import

let days : (module Solution.Day.S) list =
  [ (module Day01)
  ; (module Day02)
  ; (module Day03)
  ; (module Day04)
  ; (module Day05)
  ; (module Day06)
  ]
;;

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.map days ~f:(fun (module Day) -> Day.command))
;;
