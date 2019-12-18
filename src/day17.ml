open! Core
open! Async
open! Import

let get_grid program =
  let program = Intcode.run_program program in
  Pipe.to_list (Intcode.output program)
  >>| List.map ~f:Char.of_int_exn
  >>| String.of_char_list
  >>| String.split ~on:'\n'
  >>| List.map ~f:String.to_list
  >>| List.foldi ~init:Int_pair.Map.empty ~f:(fun y grid ->
          List.foldi ~init:grid ~f:(fun x grid elem ->
              Map.add_exn grid ~key:(x, y) ~data:elem))
;;

let scaffolds_of_grid grid =
  Map.filter grid ~f:(function
      | '#' | '^' -> true
      | _ -> false)
  |> Map.key_set
;;

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1

  let solve input =
    let%bind grid = get_grid input in
    let scaffolds = scaffolds_of_grid grid in
    let intersections =
      Set.filter scaffolds ~f:(fun scaffold ->
          List.for_all [ 1, 0; 0, 1; -1, 0; 0, -1 ] ~f:(fun shift ->
              Set.mem scaffolds (Int_pair.add scaffold shift)))
    in
    Set.to_list intersections |> List.sum (module Int) ~f:(Tuple2.uncurry ( * )) |> return
  ;;
end)

let find_robot grid =
  match
    Map.filter_map grid ~f:(function
        | '^' -> Some (0, -1)
        | 'v' -> Some (0, 1)
        | '<' -> Some (-1, 0)
        | '>' -> Some (1, 0)
        | _ -> None)
    |> Map.to_alist
  with
  | [ (position, direction) ] -> position, direction
  | robots ->
    raise_s
      [%message "Should only have one robot" (robots : ((int * int) * (int * int)) list)]
;;

module Step = struct
  type t =
    | Straight
    | Right
    | Left
  [@@deriving sexp_of, equal]

  let turn t (dx, dy) =
    match t with
    | Straight -> dx, dy
    | Right -> -dy, dx
    | Left -> dy, -dx
  ;;

  let to_char t =
    match t with
    | Straight -> 'S'
    | Right -> 'R'
    | Left -> 'L'
  ;;
end

let construct_path scaffolds robot =
  let rec loop (robot_position, robot_direction) (path : Step.t list) =
    match
      List.find_map [ Step.Straight; Right; Left ] ~f:(fun direction ->
          let robot_direction = Step.turn direction robot_direction in
          let next_position = Int_pair.add robot_position robot_direction in
          match Set.mem scaffolds next_position with
          | true -> Some (loop (next_position, robot_direction) (direction :: path))
          | false -> None)
    with
    | Some l -> l
    | None -> List.rev path
  in
  loop robot []
;;

let compress_path path =
  let rec combine (path : (Step.t * int) list) =
    match path with
    | [] -> []
    | (direction, 1) :: (Straight, n) :: rest -> (direction, n + 1) :: combine rest
    | _ -> raise_s [%message "Unexpected path shape"]
  in
  List.group path ~break:(fun a b -> not (Step.equal a b))
  |> List.map ~f:(fun l -> List.hd_exn l, List.length l)
  |> combine
;;

module Part_2 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 2
  let a : (Step.t * int) list = [ Right, 6; Right, 6; Right, 8; Left, 10; Left, 4 ]
  let b : (Step.t * int) list = [ Right, 6; Left, 10; Right, 8 ]
  let c : (Step.t * int) list = [ Left, 4; Left, 12; Right, 6; Left, 10 ]
  let main_routine = "A,B,B,A,C,A,C,A,C,B"

  let string_of_movements movements =
    List.map movements ~f:(fun (step, n) -> sprintf "%c,%d" (Step.to_char step) n)
    |> String.concat ~sep:","
  ;;

  let input_string program string =
    let ascii_codes = String.to_list string |> List.map ~f:Char.to_int in
    let input c =
      let%bind () = Intcode.awaiting_input program in
      Intcode.input program c;
      return ()
    in
    let%bind () = Deferred.List.iter ascii_codes ~f:input in
    input (Char.to_int '\n')
  ;;

  let solve input =
    let input = 2 :: List.tl_exn input in
    let program = Intcode.run_program input in
    let%bind () = input_string program main_routine in
    let%bind () =
      Deferred.List.iter
        [ a; b; c ]
        ~f:(Fn.compose (input_string program) string_of_movements)
    in
    let%bind () = input_string program "y" in
    Deferred.repeat_until_finished () (fun () ->
        match%bind Pipe.read (Intcode.output program) with
        | `Eof -> raise_s [%message "Saw EOF without dust count"]
        | `Ok output ->
          return
            (match Char.of_int output with
            | Some _ -> `Repeat ()
            | None -> `Finished output))
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 17
  let parts = [ Part_1.command; Part_2.command ]
end)
