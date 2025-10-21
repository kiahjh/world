let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 10.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 2.0 in
  let third_pass = Perlin.perlin2d (x *. 0.3) (z *. 0.3) *. 1.0 in
  first_pass +. second_pass +. third_pass

let draw_terrain size =
  let open Raylib in
  for x = 0 to size do
    for z = 0 to size do
      let height = noise (float_of_int x) (float_of_int z) in
      draw_cube
        (Vector3.create
           (float_of_int (x - (size / 2)))
           height
           (float_of_int (z - (size / 2))))
        1.0 1.0 1.0 Color.green
    done
  done
