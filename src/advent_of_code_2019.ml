open! Core
open! Import

let days : (module Solution.Day.S) list =
  [ (module Day01)
  ; (module Day02)
  ; (module Day03)
  ; (module Day04)
  ; (module Day05)
  ; (module Day06)
  ; (module Day07)
  ; (module Day08)
  ; (module Day09)
  ; (module Day10)
  ; (module Day11)
  ; (module Day12)
  ; (module Day13)
  ; (module Day14)
  ; (module Day15)
  ; (module Day16)
  ]
;;

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.map days ~f:(fun (module Day) -> Day.command))
;;
