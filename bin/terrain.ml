open Blocks

let noise x z =
  let first_octave = Perlin.perlin2d (x *. 0.005) (z *. 0.005) *. 30.0 in
  let second_octave = Perlin.perlin2d (x *. 0.01) (z *. 0.01) *. 15.0 in
  let third_octave = Perlin.perlin2d (x *. 0.02) (z *. 0.02) *. 8.0 in
  let fourth_octave = Perlin.perlin2d (x *. 0.04) (z *. 0.04) *. 4.0 in
  let fifth_octave = Perlin.perlin2d (x *. 0.08) (z *. 0.08) *. 2.0 in
  first_octave +. second_octave +. third_octave +. fourth_octave +. fifth_octave

let load_terrain size =
  print_endline "loading terrain...";
  let vertices = ref [||] in
  for x = 0 to size do
    let row = ref [||] in
    for z = 0 to size do
      let height = noise (float_of_int x) (float_of_int z) in
      row := Array.append !row [| height |]
    done;
    vertices := Array.append !vertices [| !row |]
  done;
  print_endline "terrain loaded";
  vertices

let draw_terrain size terrain water_level depth user_x user_y user_z =
  (* draw height map *)
  for x = 0 to size do
    for z = 0 to size do
      let height = !terrain.(x).(z) in
      for y = -depth to 50 do
        let yf = float_of_int y in
        let air_on_top = yf +. 1.0 >= height in
        let air_on_bottom = y - 1 < -depth in
        let air_on_north = x = size || !terrain.(x + 1).(z) < yf in
        let air_on_east = z = size || !terrain.(x).(z + 1) < yf in
        let air_on_south = x = 0 || !terrain.(x - 1).(z) < yf in
        let air_on_west = z = 0 || !terrain.(x).(z - 1) < yf in
        if yf <= height then
          draw_stone_block
            ~x:(x - (size / 2))
            ~y
            ~z:(z - (size / 2))
            ~north_exposed:air_on_north ~east_exposed:air_on_east
            ~south_exposed:air_on_south ~west_exposed:air_on_west
            ~top_exposed:air_on_top ~bottom_exposed:air_on_bottom ~user_x
            ~user_y ~user_z
        else if yf <= float_of_int water_level then
          draw_water_block
            ~x:(x - (size / 2))
            ~y
            ~z:(z - (size / 2))
            ~north_exposed:(x = size) ~east_exposed:(z = size)
            ~south_exposed:(x = 0) ~west_exposed:(z = 0)
            ~top_exposed:(y = water_level) ~bottom_exposed:false ~user_x ~user_y
            ~user_z
      done
    done
  done
