open! Core
open! Import

module Input = struct
  type t = int list

  let of_string s =
    String.to_list s |> List.map ~f:(fun digit -> Char.to_int digit - Char.to_int '0')
  ;;

  let load filename = In_channel.read_all filename |> String.strip |> of_string
end

let repeat_n_times x ~n = Sequence.take (Sequence.repeat x) n
let duplicate_each sequence ~n = Sequence.concat_map sequence ~f:(repeat_n_times ~n)

let for_nth_element n =
  let base_pattern = Sequence.cycle_list_exn [ 0; 1; 0; -1 ] in
  let repeating = duplicate_each base_pattern ~n in
  Sequence.drop_eagerly repeating 1
;;

let one_step input =
  let length = List.length input in
  List.range 0 length
  |> List.map ~f:(fun i ->
         let pattern = for_nth_element (i + 1) in
         let pattern = Sequence.take pattern length |> Sequence.to_list in
         List.zip_exn input pattern |> List.sum (module Int) ~f:(Tuple2.uncurry ( * )))
  |> List.map ~f:(fun n -> abs (n mod 10))
;;

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = String

  let one_based_index = 1

  let solve input =
    let full_result = Fn.apply_n_times ~n:100 one_step input in
    List.take full_result 8 |> List.map ~f:Int.to_string |> String.concat
  ;;
end)

let%expect_test "Small example" =
  let input = Input.of_string "12345678" in
  let (_ : int list) =
    List.range 0 4
    |> List.fold ~init:input ~f:(fun input _ ->
           let output = one_step input in
           print_s [%message (output : int list)];
           output)
  in
  [%expect
    {|
    (output (4 8 2 2 6 1 5 8))
    (output (3 4 0 4 0 4 3 8))
    (output (0 3 4 1 5 5 1 8))
    (output (0 1 0 2 9 4 9 8)) |}]
;;

let%expect_test "Part 1" =
  let inputs =
    [ "80871224585914546619083218645595"
    ; "19617804207202209144916044189917"
    ; "69317163492948606335995924319873"
    ]
  in
  List.iter inputs ~f:(fun input ->
      let output = Part_1.solve_input input in
      print_s [%message input output]);
  [%expect
    {|
    (80871224585914546619083218645595 24176176)
    (19617804207202209144916044189917 73745418)
    (69317163492948606335995924319873 52432133) |}]
;;

let compute_sneaky input =
  let offset =
    List.take input 7 |> List.map ~f:Int.to_string |> String.concat |> Int.of_string
  in
  let repeated_input = List.range 0 10_000 |> List.concat_map ~f:(fun _ -> input) in
  let tail = List.drop repeated_input offset in
  let reversed = List.rev tail in
  let one_step l =
    List.folding_map l ~init:0 ~f:(fun a b ->
        let output = abs ((a + b) mod 10) in
        output, output)
  in
  Fn.apply_n_times ~n:100 one_step reversed |> List.rev
;;

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = String

  let one_based_index = 2

  let solve input =
    let full_result = compute_sneaky input in
    List.take full_result 8 |> List.map ~f:Int.to_string |> String.concat
  ;;
end)

let%expect_test "Part 2" =
  let inputs =
    [ "03036732577212944063491565474664"
    ; "02935109699940807407585447034323"
    ; "03081770884921959731165446850517"
    ]
  in
  List.iter inputs ~f:(fun input ->
      let output = Part_2.solve_input input in
      print_s [%message input output]);
  [%expect
    {|
    (03036732577212944063491565474664 84462026)
    (02935109699940807407585447034323 78725270)
    (03081770884921959731165446850517 53553731) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 16
  let parts = [ Part_1.command; Part_2.command ]
end)
