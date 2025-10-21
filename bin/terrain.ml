open Blocks

let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 10.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 2.0 in
  let third_pass = Perlin.perlin2d (x *. 0.3) (z *. 0.3) *. 1.0 in
  first_pass +. second_pass +. third_pass

(* before optimization: 3-4 fps *)
(* after not drawing surrounded blocks: 38 fps *)

let draw_terrain size _water_level =
  let vertices = ref [||] in

  (* build up height map *)
  for x = 0 to size do
    let row = ref [||] in
    for z = 0 to size do
      let height = noise (float_of_int x) (float_of_int z) in
      row := Array.append !row [| height |]
    done;
    vertices := Array.append !vertices [| !row |]
  done;

  (* draw height map *)
  for x = 0 to size do
    for z = 0 to size do
      let height = !vertices.(x).(z) in
      for y = -50 to 50 do
        let yf = float_of_int y in
        let air_on_top = yf +. 1.0 >= height in
        let air_on_bottom = y - 1 < -50 in
        let air_on_north =
          (* left off here *)
          x == size || (!vertices.(x + 1).(z) > height && floor height == yf)
        in
        let air_on_east = false in
        let air_on_south = false in
        let air_on_west = false in
        if yf <= height then
          draw_stone_block
            ~x:(x - (size / 2))
            ~y
            ~z:(z - (size / 2))
            ~north:air_on_north ~east:air_on_east ~south:air_on_south
            ~west:air_on_west ~top:air_on_top ~bottom:air_on_bottom
      done
    done
  done
