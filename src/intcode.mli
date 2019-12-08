open! Core
open! Import

module Input : sig
  type t = int list [@@deriving sexp]

  include Input.S with type t := t
end

module Result : sig
  type t =
    { state : int list
    ; output : int list
    }
  [@@deriving sexp]
end

val run_program : int list -> input:int list -> Result.t
