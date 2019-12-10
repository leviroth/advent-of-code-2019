open! Core
open! Async
open! Import

type t = int list [@@deriving sexp]

module Input : sig
  type nonrec t = t [@@deriving sexp]

  include Input.S with type t := t
end

val run_program
  :  ?id:int
  -> t
  -> input:int Pipe.Reader.t
  -> output:int Pipe.Writer.t
  -> t Deferred.t

module Util : sig
  val sink : int Pipe.Writer.t
end
