open! Core
open! Async
open! Import

module Program = struct
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
    | Relative
  [@@deriving sexp]

  let of_int i =
    match i with
    | 0 -> Position
    | 1 -> Immediate
    | 2 -> Relative
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
    | Adjust_relative_base
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
    | 9 -> Adjust_relative_base
    | 99 -> Halt
    | _ -> raise_s [%message "Unknown opcode" (i : int)]
  ;;

  let num_parameters t =
    match t with
    | Halt -> 0
    | Input | Output | Adjust_relative_base -> 1
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

type t =
  { state : int Int.Table.t
  ; input : (unit -> [ `Eof | `Ok of int ] Deferred.t) * (int -> unit)
  ; output : int Pipe.Reader.t * int Pipe.Writer.t
  ; awaiting_input : unit Mvar.Read_write.t
  }

let awaiting_input { awaiting_input; _ } = Mvar.value_available awaiting_input

let state { state; _ } =
  Hashtbl.to_alist state
  |> List.sort ~compare:(Comparable.lift compare ~f:fst)
  |> List.map ~f:snd
;;

let input { input = _, input_writer; _ } = input_writer
let output { output = output_reader, _; _ } = output_reader
let finished { output = output_reader, _; _ } = Pipe.closed output_reader

let run_program'
    { state; input = input_reader, _; output = _, output_writer; awaiting_input }
  =
  let relative_base = ref 0 in
  let get_direct index = Hashtbl.find state index |> Option.value ~default:0 in
  let get (mode : Mode.t) index =
    get_direct
      (match mode with
      | Position -> get_direct index
      | Immediate -> index
      | Relative -> !relative_base + get_direct index)
  in
  let rec advance (instruction : Instruction.t) index =
    apply_instruction (Opcode.num_parameters instruction.opcode + index + 1)
  and apply_instruction index =
    let ({ opcode; modes } as instruction : Instruction.t) =
      Instruction.of_int (get_direct index)
    in
    let parameter offset = get instruction.modes.(offset) (index + offset + 1) in
    let set_indirect parameter_offset data =
      let parameter_index = index + parameter_offset + 1 in
      let parameter = get_direct parameter_index in
      let mode = modes.(parameter_offset) in
      let key =
        match mode with
        | Position -> parameter
        | Relative -> parameter + !relative_base
        | Immediate ->
          raise_s
            [%message
              "Unexpected Immediate mode for \"set\" parameter"
                (index : int)
                (instruction : Instruction.t)]
      in
      Hashtbl.set state ~key ~data
    in
    let apply_simple_op operator =
      let data = operator (parameter 0) (parameter 1) in
      set_indirect 2 data
    in
    match opcode with
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
      let%bind () = Mvar.put awaiting_input () in
      (match%bind input_reader () with
      | `Eof -> raise_s [%message "Unexpected EOF"]
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
    | Adjust_relative_base ->
      relative_base := !relative_base + parameter 0;
      advance instruction index
  in
  apply_instruction 0
;;

let run_program program =
  let awaiting_input = Mvar.create () in
  let input =
    let reader, writer = Pipe.create () in
    ( (fun () -> Pipe.read reader)
    , fun value ->
        let (_ : unit option) = Mvar.take_now awaiting_input in
        Pipe.write_without_pushback writer value )
  in
  let state = List.mapi program ~f:Tuple2.create |> Int.Table.of_alist_exn in
  let t = { state; input; output = Pipe.create (); awaiting_input } in
  don't_wait_for (run_program' t);
  t
;;
