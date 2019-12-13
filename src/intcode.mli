open! Core
open! Async
open! Import

type t = int list [@@deriving sexp]

module Program : sig
  type nonrec t = t [@@deriving sexp]

  include Input.S with type t := t
end

module Input_port : sig
  type t = unit -> [ `Eof | `Ok of int ] Deferred.t

  val of_pipe : int Pipe.Reader.t -> t
  val of_list : int list -> t
end

val run_program : t -> input:Input_port.t -> output:int Pipe.Writer.t -> t Deferred.t

module Util : sig
  val sink : int Pipe.Writer.t
end
