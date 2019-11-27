open! Core
open! Import
include Input_intf

module Make_parseable (T : Parser) = struct
  type t = T.t

  let of_string s = Angstrom.parse_string T.parser s |> Result.ok_or_failwith

  let load file =
    In_channel.with_file file ~f:(fun in_channel ->
        Angstrom_unix.parse T.parser in_channel |> snd |> Result.ok_or_failwith)
  ;;
end

module Make_parseable_many (T : Parser) = struct
  include Make_parseable (struct
    type t = T.t list

    let parser = Angstrom.many T.parser
  end)
end
