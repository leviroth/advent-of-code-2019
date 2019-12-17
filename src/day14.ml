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

let cost graph quantity =
  requirements { Term.chemical = "FUEL"; quantity } String.Map.empty graph |> fst
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1

  let solve input =
    let graph = Input.to_graph input in
    cost graph 1
  ;;
end)

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 2
  let ore = 1000000000000

  let solve input =
    let graph = Input.to_graph input in
    let upper_bound =
      let rec loop candidate =
        match cost graph candidate < ore with
        | false -> candidate
        | true -> loop (candidate * 2)
      in
      loop 1
    in
    let rec loop lower_bound upper_bound =
      match lower_bound <> upper_bound with
      | false -> lower_bound
      | true ->
        let candidate = (lower_bound + upper_bound) / 2 in
        (match Ordering.of_int (compare (cost graph candidate) ore) with
        | Equal -> candidate
        | Greater -> loop lower_bound (candidate - 1)
        | Less ->
          (match cost graph (candidate + 1) < ore with
          | false -> candidate
          | true -> loop (candidate + 1) upper_bound))
    in
    loop 1 upper_bound
  ;;
end)

let%expect_test "Part 1" =
  Part_1.solve_input Input.for_test |> print_endline;
  [%expect {| 31 |}]
;;

let%expect_test "Part 2" =
  let input =
    {|157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
|}
  in
  Part_2.solve_input input |> print_endline;
  [%expect {| 82892753 |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 14
  let parts = [ Part_1.command; Part_2.command ]
end)
