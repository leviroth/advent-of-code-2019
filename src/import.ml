include Core
include Int.Replace_polymorphic_compare

module Int_pair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

let pad_int = sprintf "%02d"
