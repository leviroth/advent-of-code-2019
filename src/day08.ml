open! Core
open! Import

module Input = struct
  include String

  let load = Fn.compose String.strip In_channel.read_all
end

let width = 25
let height = 6

let layers input ~width ~height =
  String.to_list input |> List.groupi ~break:(fun i _ _ -> i mod (width * height) = 0)
;;

let layer_with_fewest_zeros layers =
  List.min_elt
    layers
    ~compare:(Comparable.lift compare ~f:(List.count ~f:(Char.equal '0')))
  |> Option.value_exn
;;

let checksum layer =
  List.count layer ~f:(Char.equal '1') * List.count layer ~f:(Char.equal '2')
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve input = layers input ~width ~height |> layer_with_fewest_zeros |> checksum
end)

include Solution.Day.Make (struct
  let day_of_month = 8
  let parts = [ Part_1.command ]
end)
