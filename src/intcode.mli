open! Core
open! Async
open! Import

module Program : sig
  type t = int list [@@deriving sexp]

  include Input.S with type t := t
end

type t

val run_program : Program.t -> t
val finished : t -> unit Deferred.t
val state : t -> Program.t
val input : t -> int Pipe.Writer.t
val output : t -> int Pipe.Reader.t
