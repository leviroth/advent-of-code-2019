open! Core
open! Async
open! Import

module Direction = struct
  type t = Int_pair.t [@@deriving sexp]

  let turn_left (d_x, d_y) = -d_y, d_x
  let turn_right (d_x, d_y) = d_y, -d_x

  let%expect_test _ =
    let position = ref (1, 0) in
    for _ = 1 to 4 do
      Ref.replace position turn_left;
      let position = !position in
      print_s [%message (position : Int_pair.t)]
    done;
    for _ = 1 to 4 do
      Ref.replace position turn_right;
      let position = !position in
      print_s [%message (position : Int_pair.t)]
    done;
    [%expect
      {|
      (position (0 1))
      (position (-1 0))
      (position (0 -1))
      (position (1 0))
      (position (0 -1))
      (position (-1 0))
      (position (0 1))
      (position (1 0)) |}]
  ;;

  let turn_according_to_bit t bit =
    match bit with
    | 0 -> turn_left t
    | 1 -> turn_right t
    | _ -> raise_s [%message "Unexpected bit" (bit : int)]
  ;;
end

let bool_of_int int =
  match int with
  | 0 -> false
  | 1 -> true
  | _ -> raise_s [%message "Unexpected bit" (int : int)]
;;

module Part = struct
  type _ t =
    | One : int t
    | Two : Int_pair.Hash_set.t t
end

let do_robot_things (type a) (part : a Part.t) program =
  let position = ref (0, 0) in
  let direction = 0, 1 in
  let ever_touched = Int_pair.Hash_set.create () in
  let tiles =
    Int_pair.Hash_set.of_list
      (match part with
      | One -> []
      | Two -> [ !position ])
  in
  let bail () : a Deferred.t =
    match part with
    | One -> return (Hash_set.length ever_touched)
    | Two -> return tiles
  in
  let program = Intcode.run_program program in
  let rec loop direction =
    Pipe.write_without_pushback
      (Intcode.input program)
      (Hash_set.mem tiles !position |> Bool.to_int);
    let%bind paint = Pipe.read (Intcode.output program) in
    match paint with
    | `Eof -> bail ()
    | `Ok bit ->
      (match bool_of_int bit with
      | true -> Hash_set.add tiles !position
      | false -> Hash_set.remove tiles !position);
      Hash_set.add ever_touched !position;
      let%bind turn_bit = Pipe.read (Intcode.output program) in
      (match turn_bit with
      | `Eof -> bail ()
      | `Ok bit ->
        let new_direction = Direction.turn_according_to_bit direction bit in
        position := Int_pair.add !position new_direction;
        loop new_direction)
  in
  loop direction
;;

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1
  let solve = do_robot_things One
end)

module Part_2 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Unit

  let one_based_index = 2

  let range coordinates =
    let max = List.max_elt coordinates ~compare |> Option.value_exn in
    let min = List.min_elt coordinates ~compare |> Option.value_exn in
    min, max
  ;;

  let solve input =
    let%bind white_squares = do_robot_things Two input in
    let far_left, far_right =
      let x_coordinates = Hash_set.to_list white_squares |> List.map ~f:fst in
      range x_coordinates
    in
    let bottom, top =
      let y_coordinates = Hash_set.to_list white_squares |> List.map ~f:snd in
      range y_coordinates
    in
    List.range ~stride:(-1) ~stop:`inclusive top bottom
    |> List.iter ~f:(fun y ->
           List.range ~stop:`inclusive far_left far_right
           |> List.iter ~f:(fun x ->
                  print_char
                    (match Hash_set.mem white_squares (x, y) with
                    | true -> '#'
                    | false -> ' '));
           print_newline ());
    return ()
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 11
  let parts = [ Part_1.command; Part_2.command ]
end)
