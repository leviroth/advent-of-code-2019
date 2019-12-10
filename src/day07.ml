open! Core
open! Async
open! Import

let run_one program phase input_signal =
  let reader, writer = Pipe.create () in
  let%bind (_ : int list) =
    Intcode.run_program
      program
      ~input:(Pipe.of_list [ phase; input_signal ])
      ~output:writer
  in
  let%bind output = Pipe.to_list reader in
  return (List.hd_exn output)
;;

let run_sequence program sequence =
  Deferred.List.fold sequence ~init:0 ~f:(fun input_signal phase ->
      run_one program phase input_signal)
;;

let%expect_test "Part 1" =
  let inputs =
    [ "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", [ 4; 3; 2; 1; 0 ]
    ; ( "3,23,3,24,1002,24,10,24,1002,23,-1,23,\n101,5,23,23,1,24,23,23,4,23,99,0,0"
      , [ 0; 1; 2; 3; 4 ] )
    ; ( "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\n\
         1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
      , [ 1; 0; 4; 3; 2 ] )
    ]
  in
  let%bind () =
    Deferred.List.iter inputs ~f:(fun (program, sequence) ->
        let program = Intcode.Input.of_string program in
        let%bind output = run_sequence program sequence in
        print_s [%message (output : int)];
        return ())
  in
  [%expect {|
    (output 43210)
    (output 54321)
    (output 65210) |}]
;;

let rec permutations list =
  match list with
  | [] -> [ [] ]
  | _ ->
    List.concat_map list ~f:(fun elt ->
        List.filter list ~f:(Fn.non (Int.equal elt))
        |> permutations
        |> List.map ~f:(fun permutation -> elt :: permutation))
;;

let part_1_sequences = List.range 0 5 |> permutations

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Input
  module Output = Int

  let one_based_index = 1

  let solve program =
    let%bind opt =
      Deferred.List.map part_1_sequences ~f:(run_sequence program)
      >>| List.max_elt ~compare
    in
    return (Option.value_exn opt)
  ;;
end)

let part_2_sequences = List.range 5 10 |> permutations

let run_sequence program sequence =
  let last_value = ref None in
  let input_pipes =
    List.map sequence ~f:(fun phase ->
        let reader, writer = Pipe.create () in
        Pipe.write_without_pushback writer phase;
        reader, writer)
  in
  let _, first_writer = List.hd_exn input_pipes in
  Pipe.write_without_pushback first_writer 0;
  let output_pipes =
    List.length sequence |> List.init ~f:(fun (_ : int) -> Pipe.create ())
  in
  let return_to_front, send_to_thrusters =
    let reader, _writer = List.last_exn output_pipes in
    Pipe.fork reader ~pushback_uses:`Fast_consumer_only
  in
  don't_wait_for (Pipe.transfer_id return_to_front first_writer);
  let rec connect pipes =
    match pipes with
    | [] | [ _ ] -> return ()
    | (_, (output_reader, _)) :: (((_, input_writer), _) :: _ as rest) ->
      don't_wait_for (Pipe.transfer_id output_reader input_writer);
      connect rest
  in
  let pipes = List.zip_exn input_pipes output_pipes in
  let%bind () = connect pipes in
  let%bind () =
    Deferred.List.iter
      pipes
      ~how:`Parallel
      ~f:(fun ((input_reader, _), (_, output_writer)) ->
        Intcode.run_program program ~input:input_reader ~output:output_writer
        |> Deferred.ignore_m)
  in
  let%bind () =
    Pipe.iter send_to_thrusters ~f:(fun value ->
        last_value := Some value;
        return ())
  in
  return (Option.value_exn !last_value)
;;

let%expect_test "Part 2" =
  let inputs =
    [ ( "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\n\
         27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
      , [ 9; 8; 7; 6; 5 ] )
    ; ( "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\n\
         -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\n\
         53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
      , [ 9; 7; 8; 5; 6 ] )
    ]
  in
  let%bind () =
    Deferred.List.iter inputs ~f:(fun (program, sequence) ->
        let program = Intcode.Input.of_string program in
        let%bind output = run_sequence program sequence in
        print_s [%message (output : int)];
        return ())
  in
  [%expect {|
    (output 139629729)
    (output 18216) |}]
;;

module Part_2 = Solution.Part.Make_async (struct
  module Input = Intcode.Input
  module Output = Int

  let one_based_index = 2

  let solve program =
    let%bind opt =
      Deferred.List.map part_2_sequences ~f:(run_sequence program)
      >>| List.max_elt ~compare
    in
    return (Option.value_exn opt)
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 7
  let parts = [ Part_1.command; Part_2.command ]
end)
