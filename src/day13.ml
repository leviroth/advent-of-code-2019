open! Core
open! Async
open! Import

module Tile = struct
  type t =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball
  [@@deriving sexp, equal]

  let of_int int =
    match int with
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | _ -> raise_s [%message "Unknown tile" (int : int)]
  ;;
end

module Part_1 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 1

  let solve input =
    let grid = Int_pair.Table.create () in
    let program = Intcode.run_program input in
    let output = Intcode.output program in
    let rec loop () =
      match%bind Pipe.read_exactly output ~num_values:3 with
      | `Eof -> return (Hashtbl.count grid ~f:(Tile.equal Block))
      | `Fewer _ -> raise_s [%message "Unexpectedly ran out of values"]
      | `Exactly queue ->
        let x = Queue.dequeue_exn queue in
        let y = Queue.dequeue_exn queue in
        let tile = Queue.dequeue_exn queue |> Tile.of_int in
        Hashtbl.set grid ~key:(x, y) ~data:tile;
        loop ()
    in
    loop ()
  ;;
end)

module Game = struct
  type t =
    { grid : (Tile.t Int_pair.Table.t[@sexp.opaque])
    ; mutable paddle : Int_pair.t option
    ; mutable ball : Int_pair.t option
    ; mutable score : int
    }
  [@@deriving sexp]

  let create () =
    { grid = Int_pair.Table.create (); paddle = None; ball = None; score = 0 }
  ;;

  let paddle_direction { paddle; ball; _ } =
    let%bind.Option paddle = paddle
    and ball = ball in
    Some
      (match Ordering.of_int (compare (fst paddle) (fst ball)) with
      | Less -> 1
      | Greater -> -1
      | Equal -> 0)
  ;;
end

module Part_2 = Solution.Part.Make_async (struct
  module Input = Intcode.Program
  module Output = Int

  let one_based_index = 2

  let solve input =
    let tail = List.tl_exn input in
    let game = Game.create () in
    let program = Intcode.run_program (2 :: tail) in
    let input = Intcode.input program in
    let output = Intcode.output program in
    let read_available () =
      match Pipe.read_now' output with
      | `Eof | `Nothing_available -> raise_s [%message "Unexpectedly ran out of values"]
      | `Ok queue ->
        let rec loop' () =
          match Queue.dequeue queue with
          | None -> Game.paddle_direction game |> Option.iter ~f:input
          | Some -1 ->
            let (_ : int) = Queue.dequeue_exn queue in
            let new_score = Queue.dequeue_exn queue in
            game.score <- new_score;
            loop' ()
          | Some x ->
            let y = Queue.dequeue_exn queue in
            let tile = Queue.dequeue_exn queue |> Tile.of_int in
            Hashtbl.set game.grid ~key:(x, y) ~data:tile;
            (match tile with
            | Ball -> game.ball <- Some (x, y)
            | Paddle -> game.paddle <- Some (x, y)
            | _ -> ());
            loop' ()
        in
        loop' ()
    in
    Deferred.repeat_until_finished () (fun () ->
        Deferred.choose
          [ choice (Intcode.awaiting_input program) (fun () ->
                read_available ();
                `Repeat ())
          ; choice (Intcode.finished program) (fun () ->
                read_available ();
                `Finished game.score)
          ])
  ;;
end)

include Solution.Day.Make (struct
  let day_of_month = 13
  let parts = [ Part_1.command; Part_2.command ]
end)
