open! Core
open! Import

module Input = struct
  module Direction = struct
    type t =
      | Up
      | Down
      | Left
      | Right
    [@@deriving sexp]

    let add t distance (x, y) =
      match t with
      | Up -> x, y + distance
      | Down -> x, y - distance
      | Left -> x - distance, y
      | Right -> x + distance, y
    ;;

    let parser =
      let open Angstrom in
      any_char
      >>| function
      | 'U' -> Up
      | 'D' -> Down
      | 'L' -> Left
      | 'R' -> Right
      | c -> raise_s [%message "Unexpected direction" (c : char)]
    ;;
  end

  module Step = struct
    type t =
      { direction : Direction.t
      ; distance : int
      }
    [@@deriving sexp]

    let parser =
      let open Angstrom in
      let integer =
        take_while1 (function
            | '0' .. '9' -> true
            | _ -> false)
        >>| int_of_string
      in
      take_while (function
          | ' ' | '\n' -> true
          | _ -> false)
      *> lift2
           (fun direction distance -> { direction; distance })
           Direction.parser
           integer
    ;;
  end

  include Input.Make_parseable (struct
    type t = Step.t list * Step.t list [@@deriving sexp]

    let parser =
      let open Angstrom in
      let parse_one = sep_by (char ',') Step.parser in
      lift2 Tuple2.create (parse_one <* char '\n') parse_one
    ;;
  end)

  let%expect_test "Parser" =
    let case =
      {|
        R75,D30,R83,U83,L12,D49,R71,U7,L72
        U62,R66,U55,R34,D71,R55,D58,R83
      |}
    in
    of_string case |> sexp_of_t |> print_s;
    [%expect
      {|
      ((((direction Right) (distance 75)) ((direction Down) (distance 30))
        ((direction Right) (distance 83)) ((direction Up) (distance 83))
        ((direction Left) (distance 12)) ((direction Down) (distance 49))
        ((direction Right) (distance 71)) ((direction Up) (distance 7))
        ((direction Left) (distance 72)))
       (((direction Up) (distance 62)) ((direction Right) (distance 66))
        ((direction Up) (distance 55)) ((direction Right) (distance 34))
        ((direction Down) (distance 71)) ((direction Right) (distance 55))
        ((direction Down) (distance 58)) ((direction Right) (distance 83)))) |}]
  ;;
end

let visited_points steps =
  let points = Int_pair.Hash_set.create () in
  let run_one point ({ direction; distance } : Input.Step.t) =
    List.range ~stop:`inclusive 0 distance
    |> List.iter ~f:(fun distance ->
           Input.Direction.add direction distance point |> Hash_set.add points);
    Input.Direction.add direction distance point
  in
  let (_ : Int_pair.t) = List.fold steps ~init:(0, 0) ~f:run_one in
  points
;;

let%expect_test _ =
  let parser =
    let open Angstrom in
    sep_by (char ',') Input.Step.parser
  in
  let case = Angstrom.parse_string parser "R8,U5,L5,D3" |> Result.ok_or_failwith in
  visited_points case |> [%sexp_of: Int_pair.Hash_set.t] |> print_s;
  [%expect{|
    ((0 0) (1 0) (2 0) (3 0) (3 2) (3 3) (3 4) (3 5) (4 0) (4 5) (5 0) (5 5)
     (6 0) (6 5) (7 0) (7 5) (8 0) (8 1) (8 2) (8 3) (8 4) (8 5)) |}]
;;

let manhattan_distance (x, y) = abs x + abs y

include Solution.Day.Make (struct
  let day_of_month = 3

  module Part_1 = Solution.Part.Make (struct
    module Input = Input
    module Output = Int

    let one_based_index = 1

    let solve (steps_a, steps_b) =
      let points_a = visited_points steps_a in
      let points_b = visited_points steps_b in
      let intersection_without_origin =
        let result = Hash_set.inter points_a points_b in
        Hash_set.remove result (0, 0);
        result
      in
      Hash_set.to_list intersection_without_origin
      |> List.map ~f:manhattan_distance
      |> List.min_elt ~compare
      |> Option.value_exn
    ;;

    let%expect_test "Part 1" =
      let case = {|
      R8,U5,L5,D3
      U7,R6,D4,L4
    |} in
      Input.of_string case |> solve |> [%sexp_of: int] |> print_s;
      [%expect{| 6 |}]
    ;;
  end)

  let parts : (module Solution.Part.S) list = [ (module Part_1) ]
end)
