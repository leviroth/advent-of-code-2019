open! Core
open! Import

module type Vector = sig
  type t [@@deriving sexp, compare]

  include Container.Summable with type t := t

  val gravity : t -> other:t -> t
  val energy : t -> int
end

module Vector1 = struct
  type t = int [@@deriving sexp, compare]

  let ( + ) = ( + )
  let zero = 0
  let gravity basis ~other = Sign.of_int (other - basis) |> Sign.to_int |> ( * ) 1
  let energy t = abs t
end

module Vector3 = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving sexp, compare, fields]

  let gravity t ~other =
    { x = Vector1.gravity t.x ~other:other.x
    ; y = Vector1.gravity t.y ~other:other.y
    ; z = Vector1.gravity t.z ~other:other.z
    }
  ;;

  let energy { x; y; z } = Vector1.energy x + Vector1.energy y + Vector1.energy z
  let ( + ) a b = { x = a.x + b.x; y = a.y + b.y; z = a.z + b.z }
  let zero = { x = 0; y = 0; z = 0 }

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

module Moon (Vector : Vector) = struct
  type t =
    { position : Vector.t
    ; velocity : Vector.t
    }
  [@@deriving sexp, compare]

  let of_starting_position position = { position; velocity = Vector.zero }

  let update_for_gravity t ~other =
    { t with
      velocity = Vector.( + ) t.velocity (Vector.gravity t.position ~other:other.position)
    }
  ;;

  let advance t = { t with position = Vector.( + ) t.position t.velocity }
  let energy { position; velocity } = Vector.energy position * Vector.energy velocity

  let advance_all current =
    List.mapi current ~f:(fun i moon ->
        let others = List.filteri current ~f:(fun i' _ -> not (Int.equal i i')) in
        let with_updated_gravity =
          List.fold others ~init:moon ~f:(fun moon other ->
              update_for_gravity moon ~other)
        in
        advance with_updated_gravity)
  ;;
end

module Moon3 = Moon (Vector3)

module Input = struct
  type t = Vector3.t list

  include Input.Make_parseable_many (Vector3)
end

module Part_1 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 1

  let solve input =
    List.map input ~f:Moon3.of_starting_position
    |> Fn.apply_n_times ~n:1000 Moon3.advance_all
    |> List.sum (module Int) ~f:Moon3.energy
  ;;
end)

let%expect_test "Part 1" =
  let moons =
    {|<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>|}
    |> Input.of_string
    |> List.map ~f:Moon3.of_starting_position
  in
  let moons = Fn.apply_n_times ~n:10 Moon3.advance_all moons in
  print_s [%message (moons : Moon3.t list)];
  [%expect
    {|
    (moons
     (((position ((x 2) (y 1) (z -3))) (velocity ((x -3) (y -2) (z 1))))
      ((position ((x 1) (y -8) (z 0))) (velocity ((x -1) (y 1) (z 3))))
      ((position ((x 3) (y -6) (z 1))) (velocity ((x 3) (y 2) (z -3))))
      ((position ((x 2) (y 0) (z 4))) (velocity ((x 1) (y -1) (z -1)))))) |}];
  let energy = List.sum (module Int) moons ~f:Moon3.energy in
  print_s [%message (energy : int)];
  [%expect {| (energy 179) |}]
;;

module Part_2 = Solution.Part.Make (struct
  module Input = Input
  module Output = Int

  let one_based_index = 2

  module Moon = Moon (Vector1)

  module Moon_set = struct
    module T = struct
      type t = Moon.t list [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  let find_period positions =
    let rec loop moons seen =
      match Set.mem seen moons with
      | true -> Set.length seen
      | false ->
        let next_moons = Moon.advance_all moons in
        loop next_moons (Set.add seen moons)
    in
    loop positions Moon_set.Set.empty
  ;;

  let extract_axes moon3s =
    List.map moon3s ~f:(fun ({ position; velocity } : Moon3.t) ->
        let open Moon in
        ( { position = position.x; velocity = velocity.x }
        , { position = position.y; velocity = velocity.y }
        , { position = position.z; velocity = velocity.z } ))
    |> List.unzip3
  ;;

  let solve input =
    let moon3s = List.map input ~f:Moon3.of_starting_position in
    let x, y, z = extract_axes moon3s in
    List.reduce_exn [ find_period x; find_period y; find_period z ] ~f:lcm
  ;;
end)

let%expect_test "Part 2" =
  let moons =
    {|<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>|}
    |> Input.of_string
  in
  let period = Part_2.solve moons in
  print_s [%message (period : int)];
  [%expect {|
    (period 4686774924) |}]
;;

include Solution.Day.Make (struct
  let day_of_month = 12
  let parts = [ Part_1.command; Part_2.command ]
end)
