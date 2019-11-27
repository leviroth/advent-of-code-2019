open! Core
open! Import

module type Output = sig
  type t

  val to_string : t -> string
end

module Part_intf = struct
  module type Basic = sig
    module Input : Input.S
    module Output : Output

    val one_based_index : int
    val solve : Input.t -> Output.t
  end

  module type S = sig
    include Basic

    val solve_file : string -> string
    val solve_input : string -> string
    val command : day_of_month:int -> string * Command.t
  end

  module type Part = sig
    module type Basic = Basic
    module type S = S

    module Make : functor (_ : Basic) -> S
  end
end

module Day_intf = struct
  module type Basic = sig
    val day_of_month : int
    val parts : (module Part_intf.S) list
  end

  module type S = sig
    val day_of_month : int
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
