open! Core
open! Async
open! Import

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, enumerate]

  let opposite t =
    match t with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left
  ;;

  let to_vector t =
    match t with
    | Up -> 0, 1
    | Down -> 0, -1
    | Left -> -1, 0
    | Right -> 1, 0
  ;;

  let to_int_code t =
    match t with
    | Up -> 1
    | Down -> 2
    | Left -> 3
    | Right -> 4
  ;;
end

let go_in_direction program direction =
  let input = Intcode.input program in
  input (Direction.to_int_code direction);
  let output = Intcode.output program in
  match%bind Pipe.read output with
  | `Eof -> raise_s [%message "Unexpected EOF"]
  | `Ok 0 -> return `Blocked
  | `Ok 1 -> return `Moved
  | `Ok 2 -> return `Finished
  | `Ok n -> raise_s [%message "Unexpected move response" (n : int)]
;;

let find_target program =
  let run_from_origin max_depth =
    let program = Intcode.run_program program in
    let visited = Int_pair.Hash_set.create () in
    let rec loop position depth =
      match Hash_set.mem visited position || depth > max_depth with
      | true -> return None
      | false ->
        Hash_set.add visited position;
        Deferred.List.find_map Direction.all ~f:(fun direction ->
            match%bind go_in_direction program direction with
            | `Blocked -> return None
            | `Finished -> return (Some (depth + 1))
            | `Moved ->
              (match%bind
                 loop (Int_pair.add position (Direction.to_vector direction)) (depth + 1)
               with
              | None ->
                let%bind _ = go_in_direction program (Direction.opposite direction) in
                return None
              | Some answer -> return (Some answer)))
    in
    loop (0, 0) 0
  in
  Deferred.repeat_until_finished 1 (fun max_depth ->
      match%bind run_from_origin max_depth with
      | None -> return (`Repeat (max_depth + 1))
      | Some answer -> return (`Finished answer))
;;

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1
  let solve = find_target
end)

include Solution.Day.Make (struct
  let day_of_month = 15
  let parts = [ Part_1.command ]
end)
