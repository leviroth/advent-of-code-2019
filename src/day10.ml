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
;;

let base_location_with_count ({ asteroids; width; height } : Input.t) =
  Set.to_list asteroids
  |> List.map ~f:(fun start ->
         start, visible_from_one start asteroids width height |> Set.length)
  |> List.max_elt ~compare:(Comparable.lift compare ~f:snd)
  |> Option.value_exn
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1
  let solve = Fn.compose snd base_location_with_count
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

let angle_of_vector (col, row) =
  let open Float in
  let col, row = of_int col, of_int row in
  atan (row / col)
  +
  match col < zero with
  | true -> 0.5 * pi
  | false -> zero
;;

let order_of_removal start asteroids width height =
  let rec loop asteroids acc =
    match Set.is_empty asteroids with
    | true -> List.rev acc
    | false ->
      let removed = visible_from_one start asteroids width height in
      let sorted =
        Set.to_list removed
        |> List.sort
             ~compare:
               (Comparable.lift Float.compare ~f:(fun target ->
                    angle_of_vector (Int_pair.sub target start)))
      in
      loop (Set.diff asteroids removed) (List.rev_append sorted acc)
  in
  loop (Set.remove asteroids start) []
;;

let%expect_test "order_of_removal" =
  let ({ asteroids; width; height } : Input.t) =
    Input.of_string
      {|
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##
|}
  in
  let start = 8, 3 in
  let output = order_of_removal start asteroids width height in
  print_s [%message (output : Int_pair.t list)];
  [%expect
    {|
    (output
     ((8 1) (9 0) (9 1) (10 0) (9 2) (11 1) (12 1) (11 2) (15 1) (12 2) (13 2)
      (14 2) (15 2) (12 3) (16 4) (15 4) (10 4) (4 4) (2 4) (2 3) (0 2) (1 2)
      (0 1) (1 1) (5 2) (1 0) (5 1) (6 1) (6 0) (7 0) (8 0) (10 1) (14 0)
      (16 1) (13 3) (14 3))) |}]
;;

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 2

  let solve ({ asteroids; width; height } as input : Input.t) =
    let base_location = fst (base_location_with_count input) in
    let order = order_of_removal base_location asteroids width height in
    let col, row = List.nth_exn order 199 in
    (100 * col) + row
  ;;
end)

let%expect_test "Part 2" =
  let input =
    {|
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##|}
  in
  let answer = Input.of_string input |> Part_2.solve in
  print_s [%message (answer : int)];
  [%expect {| (answer 802) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 10
  let parts = [ Part_1.command; Part_2.command ]
end)
