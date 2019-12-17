open! Core
open! Import

module Term = struct
  type t =
    { quantity : int
    ; chemical : string
    }
  [@@deriving sexp, fields]

  let scale t n = { t with quantity = t.quantity * n }

  let parser =
    let open Angstrom in
    let chemical = take_while1 Char.is_uppercase in
    let integer =
      take_while1 (function
          | '0' .. '9' | '-' -> true
          | _ -> false)
      >>| int_of_string
    in
    lift2 (fun quantity chemical -> { quantity; chemical }) (integer <* char ' ') chemical
  ;;
end

module Reaction = struct
  type t =
    { inputs : Term.t list
    ; output : Term.t
    }
  [@@deriving sexp, fields]

  let parser =
    let open Angstrom in
    lift2
      (fun inputs output -> { inputs; output })
      (sep_by1 (string ", ") Term.parser)
      (string " => " *> Term.parser <* char '\n')
  ;;
end

module Input = struct
  type t = Reaction.t list [@@deriving sexp]

  include Input.Make_parseable_many (Reaction)

  let to_graph t =
    List.map t ~f:(fun reaction -> Term.chemical (Reaction.output reaction), reaction)
    |> String.Map.of_alist_exn
  ;;

  let for_test =
    {|10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
|}
  ;;
end

let%expect_test "Parsing" =
  let input = Input.of_string Input.for_test in
  print_s [%message (input : Input.t)];
  [%expect
    {|
    (input
     (((inputs (((quantity 10) (chemical ORE))))
       (output ((quantity 10) (chemical A))))
      ((inputs (((quantity 1) (chemical ORE))))
       (output ((quantity 1) (chemical B))))
      ((inputs (((quantity 7) (chemical A)) ((quantity 1) (chemical B))))
       (output ((quantity 1) (chemical C))))
      ((inputs (((quantity 7) (chemical A)) ((quantity 1) (chemical C))))
       (output ((quantity 1) (chemical D))))
      ((inputs (((quantity 7) (chemical A)) ((quantity 1) (chemical D))))
       (output ((quantity 1) (chemical E))))
      ((inputs (((quantity 7) (chemical A)) ((quantity 1) (chemical E))))
       (output ((quantity 1) (chemical FUEL)))))) |}]
;;

let reactions_needed required_quantity reaction_quantity =
  Float.(of_int required_quantity / of_int reaction_quantity |> round_up |> to_int)
;;

let consume_resources ({ chemical; quantity } as term : Term.t) resources =
  match Map.find resources chemical with
  | None -> Some term, resources
  | Some n ->
    let new_term : Term.t option =
      match quantity <= n with
      | true -> None
      | false -> Some { chemical; quantity = quantity - n }
    in
    let new_resources =
      match n <= quantity with
      | true -> Map.remove resources chemical
      | false -> Map.set resources ~key:chemical ~data:(n - quantity)
    in
    new_term, new_resources
;;

let rec requirements term resources graph =
  match Term.chemical term with
  | "ORE" -> Term.quantity term, resources
  | chemical ->
    (match consume_resources term resources with
    | None, resources -> 0, resources
    | Some term, resources ->
      let reaction : Reaction.t = Map.find_exn graph (Term.chemical term) in
      let reactions_needed =
        reactions_needed (Term.quantity term) reaction.output.quantity
      in
      let spare = (reactions_needed * reaction.output.quantity) - Term.quantity term in
      let ore, leftovers =
        List.fold
          (Reaction.inputs reaction)
          ~init:(0, resources)
          ~f:(fun (ore, resources) term ->
            let ore_needed, leftovers =
              requirements (Term.scale term reactions_needed) resources graph
            in
            ore + ore_needed, leftovers)
      in
      ( ore
      , Map.update leftovers chemical ~f:(function
            | None -> spare
            | Some n -> n + spare) ))
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1

  let solve input =
    let graph = Input.to_graph input in
    requirements { Term.chemical = "FUEL"; quantity = 1 } String.Map.empty graph |> fst
  ;;
end)

let%expect_test "Part 1" =
  Part_1.solve_input Input.for_test |> print_endline;
  [%expect {| 31 |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 14
  let parts = [ Part_1.command ]
end)
