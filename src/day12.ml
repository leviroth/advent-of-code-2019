open! Core
open! Import

let gravity_for_one_basis basis basis' =
  Sign.of_int (basis' - basis) |> Sign.to_int |> ( * ) 1
;;

module Vector = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving sexp]

  let add a b = { x = a.x + b.x; y = a.y + b.y; z = a.z + b.z }

  let gravity t ~other =
    { x = gravity_for_one_basis t.x other.x
    ; y = gravity_for_one_basis t.y other.y
    ; z = gravity_for_one_basis t.z other.z
    }
  ;;

  let parser =
    let open Angstrom in
    let integer =
      take_while1 (function
          | '0' .. '9' | '-' -> true
          | _ -> false)
      >>| int_of_string
    in
    let coordinate c = char c *> char '=' *> integer in
    lift3
      (fun x y z -> { x; y; z })
      (char '<' *> coordinate 'x' <* string ", ")
      (coordinate 'y' <* string ", ")
      (coordinate 'z' <* char '>' <* skip_many (char '\n'))
  ;;
end

module Moon = struct
  type t =
    { position : Vector.t
    ; velocity : Vector.t
    }
  [@@deriving sexp]

  let update_for_gravity t ~other =
    { t with
      velocity = Vector.add t.velocity (Vector.gravity t.position ~other:other.position)
    }
  ;;

  let advance t = { t with position = Vector.add t.position t.velocity }

  let energy { position; velocity } =
    let energy ({ x; y; z } : Vector.t) = abs x + abs y + abs z in
    energy position * energy velocity
  ;;
end

let advance_all current =
  List.mapi current ~f:(fun i moon ->
      let others = List.filteri current ~f:(fun i' _ -> i <> i') in
      let with_updated_gravity =
        List.fold others ~init:moon ~f:(fun moon other ->
            Moon.update_for_gravity moon ~other)
      in
      Moon.advance with_updated_gravity)
;;

let starting_velocity : Vector.t = { x = 0; y = 0; z = 0 }

module Input = struct
  type t = Vector.t list

  include Input.Make_parseable_many (Vector)
end

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1

  let solve input =
    List.map input ~f:(fun position ->
        ({ position; velocity = starting_velocity } : Moon.t))
    |> Fn.apply_n_times ~n:1000 advance_all
    |> List.sum (module Int) ~f:Moon.energy
  ;;
end)

let%expect_test "Part 1" =
  let moons =
    {|<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>|}
    |> Input.of_string
    |> List.map ~f:(fun position -> ({ position; velocity = starting_velocity } : Moon.t))
  in
  let moons = Fn.apply_n_times ~n:10 advance_all moons in
  print_s [%message (moons : Moon.t list)];
  [%expect
    {|
    (moons
     (((position ((x 2) (y 1) (z -3))) (velocity ((x -3) (y -2) (z 1))))
      ((position ((x 1) (y -8) (z 0))) (velocity ((x -1) (y 1) (z 3))))
      ((position ((x 3) (y -6) (z 1))) (velocity ((x 3) (y 2) (z -3))))
      ((position ((x 2) (y 0) (z 4))) (velocity ((x 1) (y -1) (z -1)))))) |}];
  let energy = List.sum (module Int) moons ~f:Moon.energy in
  print_s [%message (energy : int)];
  [%expect {| (energy 179) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 12
  let parts = [ Part_1.command ]
end)
