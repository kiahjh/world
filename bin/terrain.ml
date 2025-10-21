open Blocks

let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 10.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 2.0 in
  let third_pass = Perlin.perlin2d (x *. 0.3) (z *. 0.3) *. 1.0 in
  first_pass +. second_pass +. third_pass

let draw_terrain size _water_level user_x user_y user_z =
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
        let air_on_north = x == size || !vertices.(x + 1).(z) < yf in
        let air_on_east = z == size || !vertices.(x).(z + 1) < yf in
        let air_on_south = x == 0 || !vertices.(x - 1).(z) < yf in
        let air_on_west = z == 0 || !vertices.(x).(z - 1) < yf in
        if yf <= height then
          draw_stone_block
            ~x:(x - (size / 2))
            ~y
            ~z:(z - (size / 2))
            ~north_exposed:air_on_north ~east_exposed:air_on_east
            ~south_exposed:air_on_south ~west_exposed:air_on_west
            ~top_exposed:air_on_top ~bottom_exposed:air_on_bottom ~user_x
            ~user_y ~user_z
      done
    done
  done
