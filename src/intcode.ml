open! Core
open! Async
open! Import

type t = int list [@@deriving sexp]

module Input = struct
  module T = struct
    type nonrec t = t [@@deriving sexp]

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

module Result = struct
  type t =
    { state : int list
    ; output : int list
    }
  [@@deriving sexp]
end

let n = ref 0

let run_program' ?id program input_reader output_writer =
  let get (mode : Mode.t) index =
    program.((match mode with
             | Position -> program.(index)
             | Immediate -> index))
  in
  let rec advance (instruction : Instruction.t) index =
    apply_instruction (Opcode.num_parameters instruction.opcode + index + 1)
  and apply_instruction index =
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
    | Halt ->
      Pipe.close output_writer;
      return ()
    | Input ->
      incr n;
      (match%bind Pipe.read input_reader with
      | `Eof -> raise_s [%message "Unexpected EOF" (n : int ref) (id : int option)]
      | `Ok input ->
        set_indirect 0 input;
        advance instruction index)
    | Output ->
      let output = parameter 0 in
      Pipe.write_without_pushback output_writer output;
      advance instruction index
    | Jump_if_true ->
      (match parameter 0 with
      | 0 -> advance instruction index
      | _ -> apply_instruction (parameter 1))
    | Jump_if_false ->
      (match parameter 0 with
      | 0 -> apply_instruction (parameter 1)
      | _ -> advance instruction index)
    | Less_than ->
      set_indirect 2 (Bool.to_int (parameter 0 < parameter 1));
      advance instruction index
    | Equals ->
      set_indirect 2 (Bool.to_int (parameter 0 = parameter 1));
      advance instruction index
  in
  apply_instruction 0
;;

let run_program ?id program ~input ~output =
  let program = Array.of_list program in
  let%bind () = run_program' ?id program input output in
  return (Array.to_list program)
;;

module Util = struct
  let sink = Pipe.create_writer (fun _ -> return ())
end
