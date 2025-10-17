let noise x z =
  let first_pass = Perlin.perlin2d (x *. 0.03) (z *. 0.03) *. 15.0 in
  let second_pass = Perlin.perlin2d (x *. 0.1) (z *. 0.1) *. 5.0 in
  first_pass +. second_pass

let draw_terrain size =
  let open Raylib in
  let vertices = ref [] in

  for x = -(size / 2) to size / 2 do
    for z = -(size / 2) to size / 2 do
      let height = noise (float_of_int x) (float_of_int z) in
      vertices :=
        Vector3.create (float_of_int x) height (float_of_int z) :: !vertices
    done
  done;

  List.iter (fun v -> draw_cube v 0.1 0.1 0.1 Color.green) !vertices;

  List.iter
    (fun v ->
      if
        not
          (Vector3.x v = float_of_int (size / 2)
          || Vector3.z v = float_of_int (size / 2))
      then (
        let vertex_right =
          List.find
            (fun other ->
              Vector3.x other = Vector3.x v +. 1.0
              && Vector3.z other = Vector3.z v)
            !vertices
        in
        let vertex_below =
          List.find
            (fun other ->
              Vector3.x other = Vector3.x v
              && Vector3.z other = Vector3.z v +. 1.0)
            !vertices
        in
        let _vertex_below_right =
          List.find
            (fun other ->
              Vector3.x other = Vector3.x v +. 1.0
              && Vector3.z other = Vector3.z v +. 1.0)
            !vertices
        in
        draw_triangle_3d v vertex_below vertex_right Color.red;
        ()))
    !vertices
