open! Core
open! Async
open! Import

module type Output = sig
  type t

  val to_string : t -> string
end

module Part_intf = struct
  module With_wrapped_types (Wrapper : T1) = struct
    module type Basic = sig
      module Input : Input.S
      module Output : Output

      val one_based_index : int
      val solve : Input.t -> Output.t Wrapper.t
    end

    module type S = sig
      include Basic

      val solve_file : string -> string Wrapper.t
      val solve_input : string -> string Wrapper.t
      val command : day_of_month:int -> string * Command.t
    end
  end

  module Synchronous = With_wrapped_types (Monad.Ident)
  module Asynchronous = With_wrapped_types (Deferred)

  module type Part = sig
    module Make : functor (_ : Synchronous.Basic) -> Synchronous.S
    module Make_async : functor (_ : Asynchronous.Basic) -> Asynchronous.S
  end
end

module Day_intf = struct
  module type Basic = sig
    val day_of_month : int
    val parts : (day_of_month:int -> string * Command.t) list
  end

  module type S = sig
    include Basic

    val command : string * Command.t
  end

  module type Day = sig
    module type Basic = Basic
    module type S = S

    module Make : functor (_ : Basic) -> S
  end
end

module type Solution = sig
  module Part : Part_intf.Part
  module Day : Day_intf.Day
end
