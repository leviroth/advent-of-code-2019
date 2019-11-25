open! Core

module Int_list = struct
  type t = int list

  let of_string s = String.split_lines s |> List.map ~f:Int.of_string
  let load file = Sexp.load_sexps_conv_exn file Int.t_of_sexp
end

module String_list = struct
  type t = string list

  let of_string s = String.split_lines s
  let load file = Sexp.load_sexps_conv_exn file String.t_of_sexp
end

module String = struct
  type t = string

  let of_string s = s
  let load file = Sexp.load_sexp_conv_exn file String.t_of_sexp
end
