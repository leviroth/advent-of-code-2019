open! Core
open! Async
open! Import

let asteroid_set rows =
  List.concat_mapi rows ~f:(fun row_index row ->
      List.concat_mapi row ~f:(fun col_index element ->
          match element with
          | '.' -> []
          | '#' -> [ col_index, row_index ]
          | _ ->
            raise_s
              [%message
                "Unexpected map element"
                  (element : char)
                  (col_index : int)
                  (row_index : int)]))
  |> Int_pair.Set.of_list
;;

module Input = struct
  type t =
    { asteroids : Int_pair.Set.t
    ; width : int
    ; height : int
    }

  let of_string string =
    let rows =
      String.strip string
      |> String.split ~on:'\n'
      |> List.map ~f:(Fn.compose String.to_list String.strip)
    in
    { height = List.length rows
    ; width = List.length (List.hd_exn rows)
    ; asteroids = asteroid_set rows
    }
  ;;

  let load file = In_channel.read_all file |> of_string
end

let in_bounds (col, row) width height =
  0 <= col && col < width && 0 <= row && row < height
;;

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)
;;

let asteroids_to_check start blocker width height =
  let diff =
    let diff_col, diff_row = Int_pair.sub blocker start in
    let gcd = gcd diff_col diff_row in
    diff_col / abs gcd, diff_row / abs gcd
  in
  let rec loop current acc =
    match in_bounds current width height with
    | false -> acc
    | true -> loop (Int_pair.add current diff) (current :: acc)
  in
  loop (Int_pair.add blocker diff) []
;;

let visible_from_one start asteroids width height =
  let other_asteroids = Set.remove asteroids start in
  Set.fold other_asteroids ~init:other_asteroids ~f:(fun asteroids blocker ->
      let to_check = asteroids_to_check start blocker width height in
      List.fold to_check ~init:asteroids ~f:Set.remove)
  |> Set.length
;;

let most_visible ({ asteroids; width; height } : Input.t) =
  Set.to_list asteroids
  |> List.map ~f:(fun start -> visible_from_one start asteroids width height)
  |> List.max_elt ~compare
  |> Option.value_exn
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve = most_visible
end)

let%expect_test _ =
  let test_one_case input =
    let answer = Input.of_string input |> Part_1.solve in
    print_s [%message (answer : int)]
  in
  let cases =
    [ {|
      .#..#
      .....
      #####
      ....#
      ...##
    |}
    ; {|......#.#.
        #..#.#....
        ..#######.
        .#.#.###..
        .#..#.....
        ..#....#.#
        #..#....#.
        .##.#..###
        ##...#..#.
        .#....####
      |}
    ; {|
      #.#...#.#.
      .###....#.
      .#....#...
      ##.#.#.#.#
      ....#.#.#.
      .##..###.#
      ..#...##..
      ..##....##
      ......#...
      .####.###.
|}
    ]
  in
  List.iter cases ~f:test_one_case;
  [%expect {|
    (answer 8)
    (answer 33)
    (answer 35) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 10
  let parts = [ Part_1.command ]
end)
