let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 15.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 5.0 in
  first_pass +. second_pass

let draw_terrain size resolution =
  let open Raylib in
  let vertices = ref [||] in

  let scaling_factor = 1.0 /. resolution in

  for
    x = int_of_float (-.(float_of_int size /. 2.0 *. scaling_factor))
    to int_of_float (float_of_int size /. 2.0 *. scaling_factor)
  do
    let row = ref [||] in
    for
      z = int_of_float (-.(float_of_int size /. 2.0 *. scaling_factor))
      to int_of_float (float_of_int size /. 2.0 *. scaling_factor)
    do
      let height =
        noise (float_of_int x *. resolution) (float_of_int z *. resolution)
      in
      row := Array.append !row [| height |]
    done;
    vertices := Array.append !vertices [| !row |]
  done;

  let scaled_size = int_of_float (float_of_int size *. scaling_factor) in

  for x = 0 to scaled_size do
    for z = 0 to scaled_size do
      let height = !vertices.(x).(z) in
      if x <> scaled_size && z <> scaled_size then (
        let height_right = !vertices.(x + 1).(z) in
        let height_below = !vertices.(x).(z + 1) in
        let height_below_right = !vertices.(x + 1).(z + 1) in
        let translate point =
          (float_of_int point *. resolution) -. (float_of_int size /. 2.0)
        in
        draw_triangle_3d
          (Vector3.create (translate x) height (translate z))
          (Vector3.create (translate x) height_below (translate (z + 1)))
          (Vector3.create (translate (x + 1)) height_right (translate z))
          Color.green;
        draw_triangle_3d
          (Vector3.create (translate x) height_below (translate (z + 1)))
          (Vector3.create
             (translate (x + 1))
             height_below_right
             (translate (z + 1)))
          (Vector3.create (translate (x + 1)) height_right (translate z))
          Color.darkgreen)
    done
  done
