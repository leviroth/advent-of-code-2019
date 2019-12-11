open! Core

module Int_pair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)

  let add (a_1, b_1) (a_2, b_2) = a_1 + a_2, b_1 + b_2
  let sub (a_1, b_1) (a_2, b_2) = a_1 - a_2, b_1 - b_2
end

let pad_int = sprintf "%02d"
