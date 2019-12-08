open! Core
open! Import

module Input = struct
  module T = struct
    type t = int list [@@deriving sexp]

    let parser =
      let open Angstrom in
      let whitespace = take_while Char.is_whitespace in
      let integer =
        take_while1 (function
            | '0' .. '9' | '-' -> true
            | _ -> false)
        >>| int_of_string
      in
      sep_by (char ',') (whitespace *> integer <* whitespace)
    ;;
  end

  include T
  include Input.Make_parseable (T)
end

module Mode = struct
  type t =
    | Position
    | Immediate
  [@@deriving sexp]

  let of_int i =
    match i with
    | 0 -> Position
    | 1 -> Immediate
    | _ -> raise_s [%message "Unknown parameter mode" (i : int)]
  ;;
end

module Opcode = struct
  type t =
    | Add
    | Multiply
    | Input
    | Output
    | Jump_if_true
    | Jump_if_false
    | Less_than
    | Equals
    | Halt
  [@@deriving sexp]

  let of_int i =
    match i with
    | 1 -> Add
    | 2 -> Multiply
    | 3 -> Input
    | 4 -> Output
    | 5 -> Jump_if_true
    | 6 -> Jump_if_false
    | 7 -> Less_than
    | 8 -> Equals
    | 99 -> Halt
    | _ -> raise_s [%message "Unknown opcode" (i : int)]
  ;;

  let num_parameters t =
    match t with
    | Halt -> 0
    | Input | Output -> 1
    | Jump_if_true | Jump_if_false -> 2
    | Add | Multiply | Less_than | Equals -> 3
  ;;
end

module Instruction = struct
  type t =
    { opcode : Opcode.t
    ; modes : Mode.t array
    }
  [@@deriving sexp]

  let of_int i =
    let opcode = Opcode.of_int (i mod 100) in
    let modes =
      let rec loop acc n required =
        match required = 0 with
        | true -> Array.of_list (List.rev acc)
        | false ->
          let mode = Mode.of_int (n mod 10) in
          loop (mode :: acc) (n / 10) (required - 1)
      in
      loop [] (i / 100) (Opcode.num_parameters opcode)
    in
    { opcode; modes }
  ;;
end

let%expect_test "Instruction parsing" =
  Instruction.of_int 1002 |> Instruction.sexp_of_t |> print_s;
  [%expect {| ((opcode Multiply) (modes (Position Immediate Position))) |}]
;;

let run_program program inputs =
  let outputs = Queue.create () in
  let get (mode : Mode.t) index =
    program.((match mode with
             | Position -> program.(index)
             | Immediate -> index))
  in
  let rec advance (instruction : Instruction.t) index =
    solve (Opcode.num_parameters instruction.opcode + index + 1)
  and solve index =
    let instruction = Instruction.of_int program.(index) in
    let parameter offset = get instruction.modes.(offset) (index + offset + 1) in
    let set_indirect parameter_offset value =
      program.(program.(index + parameter_offset + 1)) <- value
    in
    let apply_simple_op operator =
      let value = operator (parameter 0) (parameter 1) in
      set_indirect 2 value
    in
    match instruction.opcode with
    | Add ->
      apply_simple_op ( + );
      advance instruction index
    | Multiply ->
      apply_simple_op ( * );
      advance instruction index
    | Halt -> ()
    | Input ->
      set_indirect 0 (Queue.dequeue_exn inputs);
      advance instruction index
    | Output ->
      let output = parameter 0 in
      print_s [%message (output : int)];
      Queue.enqueue outputs output;
      advance instruction index
    | Jump_if_true ->
      (match parameter 0 with
      | 0 -> advance instruction index
      | _ -> solve (parameter 1))
    | Jump_if_false ->
      (match parameter 0 with
      | 0 -> solve (parameter 1)
      | _ -> advance instruction index)
    | Less_than ->
      set_indirect 2 (Bool.to_int (parameter 0 < parameter 1));
      advance instruction index
    | Equals ->
      set_indirect 2 (Bool.to_int (parameter 0 = parameter 1));
      advance instruction index
  in
  solve 0;
  outputs
;;

let run_program program inputs =
  let inputs = Queue.of_list inputs in
  run_program (Array.of_list program) inputs |> Queue.last_exn
;;

let%expect_test "Part 2" =
  let program =
    {|
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
|}
    |> Input.of_string
  in
  let test_cases = [ 3; 7; 8; 9; 100 ] in
  List.iter test_cases ~f:(fun input ->
      print_s [%message (input : int)];
      let output = run_program program [ input ] in
      print_s [%message (output : int)]);
  [%expect
    {|
    (input 3)
    (output 999)
    (output 999)
    (input 7)
    (output 999)
    (output 999)
    (input 8)
    (output 1000)
    (output 1000)
    (input 9)
    (output 1001)
    (output 1001)
    (input 100)
    (output 1001)
    (output 1001) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 5

  module Part_1 = Solution.Part.Make (struct
    module Input = Input
    module Output = Int

    let one_based_index = 1
    let solve program = run_program program [ 1 ]
  end)

  module Part_2 = Solution.Part.Make (struct
    module Input = Input
    module Output = Int

    let one_based_index = 2
    let solve program = run_program program [ 5 ]
  end)

  let parts : (module Solution.Part.S) list = [ (module Part_1); (module Part_2) ]
end)
