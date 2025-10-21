open Raylib

let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 10.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 2.0 in
  let third_pass = Perlin.perlin2d (x *. 0.3) (z *. 0.3) *. 1.0 in
  first_pass +. second_pass +. third_pass

let draw_block x y z color =
  draw_cube
    (Vector3.create (float_of_int x) (float_of_int y) (float_of_int z))
    1.0 1.0 1.0 color

let draw_stone_block x y z = draw_block x y z Color.gray
let draw_water_block x y z = draw_block x y z (Color.create 0 0 255 100)

let draw_terrain size water_level =
  for x = 0 to size do
    for z = 0 to size do
      let height = noise (float_of_int x) (float_of_int z) in
      for y = -50 to 50 do
        if float_of_int y <= height then
          draw_stone_block (x - (size / 2)) y (z - (size / 2))
        else if y <= water_level then
          draw_water_block (x - (size / 2)) y (z - (size / 2))
      done
    done
  done
