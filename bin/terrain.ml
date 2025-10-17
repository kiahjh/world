let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 15.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 5.0 in
  first_pass +. second_pass

let draw_terrain size =
  let open Raylib in
  let vertices = ref [||] in

  for x = -(size / 2) to size / 2 do
    let row = ref [||] in
    for z = -(size / 2) to size / 2 do
      let height = noise (float_of_int x) (float_of_int z) in
      row := Array.append !row [| height |]
    done;
    vertices := Array.append !vertices [| !row |]
  done;

  for x = 0 to size do
    for z = 0 to size do
      let height = !vertices.(x).(z) in
      if x <> size && z <> size then (
        let height_right = !vertices.(x + 1).(z) in
        let height_below = !vertices.(x).(z + 1) in
        let height_below_right = !vertices.(x + 1).(z + 1) in
        draw_triangle_3d
          (Vector3.create (float_of_int x) height (float_of_int z))
          (Vector3.create (float_of_int x) height_below (float_of_int (z + 1)))
          (Vector3.create (float_of_int (x + 1)) height_right (float_of_int z))
          Color.green;
        draw_triangle_3d
          (Vector3.create (float_of_int x) height_below (float_of_int (z + 1)))
          (Vector3.create
             (float_of_int (x + 1))
             height_below_right
             (float_of_int (z + 1)))
          (Vector3.create (float_of_int (x + 1)) height_right (float_of_int z))
          Color.darkgreen)
    done
  done
