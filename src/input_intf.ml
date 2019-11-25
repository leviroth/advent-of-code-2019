open! Import

module type S = sig
  type t

  val of_string : string -> t
  val load : string -> t
end

module type Parser = sig
  type t

  val parser : t Angstrom.t
end

module type Input = sig
  module type S = S
  module type Parser = Parser

  module Make_parseable (T : Parser) : S with type t = T.t
  module Make_parseable_many (T : Parser) : S with type t = T.t list
end
