open! Core
open! Import

module Input = Input.Make_parseable (struct
  type t = int list [@@deriving sexp]

  let parser =
    let open Angstrom in
    let integer =
      take_while1 (function
          | '0' .. '9' | '-' -> true
          | _ -> false)
      >>| int_of_string
    in
    sep_by (char ',') integer
  ;;
end)

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
    | Halt
    | Input
    | Output
  [@@deriving sexp]

  let of_int i =
    match i with
    | 1 -> Add
    | 2 -> Multiply
    | 3 -> Input
    | 4 -> Output
    | 99 -> Halt
    | _ -> raise_s [%message "Unknown opcode" (i : int)]
  ;;

  let num_parameters t =
    match t with
    | Halt -> 0
    | Input | Output -> 1
    | Add | Multiply -> 3
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
  let rec next_index (instruction : Instruction.t) index =
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
      next_index instruction index
    | Multiply ->
      apply_simple_op ( * );
      next_index instruction index
    | Halt -> ()
    | Input ->
      set_indirect 0 (Queue.dequeue_exn inputs);
      next_index instruction index
    | Output ->
      let output = parameter 0 in
      print_s [%message (output : int)];
      Queue.enqueue outputs output;
      next_index instruction index
  in
  solve 0;
  outputs
;;

include Solution.Day.Make (struct
  let day_of_month = 5

  module Part_1 = Solution.Part.Make (struct
    module Input = Input
    module Output = Int

    let one_based_index = 1

    let solve program =
      let inputs = Queue.of_list [ 1 ] in
      run_program (Array.of_list program) inputs |> Queue.last_exn
    ;;
  end)

  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
