open Raylib
open Raylib.Rlgl

(*
  +x -> North
  -x -> South
  +z -> East
  -z -> West
  +y -> Top
  -y -> Bottom
*)

let cube ~north_face ~east_face ~south_face ~west_face ~top_face ~bottom_face =
  let draw_cube ~x ~y ~z ~north_exposed ~east_exposed ~south_exposed
      ~west_exposed ~top_exposed ~bottom_exposed ~user_x ~user_y ~user_z =
    let xf = float_of_int x in
    let yf = float_of_int y in
    let zf = float_of_int z in
    if north_exposed && user_x > xf then (
      push_matrix ();
      translatef xf yf zf;
      rotatef (-90.0) 0.0 0.0 1.0;
      north_face ();
      pop_matrix ());
    if east_exposed && user_z > zf then (
      push_matrix ();
      translatef xf yf zf;
      rotatef 90.0 1.0 0.0 0.0;
      east_face ();
      pop_matrix ());
    if south_exposed && user_x < xf then (
      push_matrix ();
      translatef xf yf zf;
      rotatef 90.0 0.0 0.0 1.0;
      south_face ();
      pop_matrix ());
    if west_exposed && user_z < zf then (
      push_matrix ();
      translatef xf yf zf;
      rotatef (-90.0) 1.0 0.0 0.0;
      west_face ();
      pop_matrix ());
    if top_exposed && user_y > yf then (
      push_matrix ();
      translatef xf yf zf;
      top_face ();
      pop_matrix ());
    if bottom_exposed && user_y < yf then (
      push_matrix ();
      translatef xf yf zf;
      rotatef 180.0 1.0 0.0 0.0;
      bottom_face ();
      pop_matrix ())
  in
  draw_cube

let draw_stone_block =
  cube
    ~north_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 105 105 105 255))
    ~east_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 100 100 100 255))
    ~south_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 105 105 105 255))
    ~west_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 100 100 100 255))
    ~top_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 115 115 115 255))
    ~bottom_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 100 100 100 255))

let draw_water_block =
  cube
    ~north_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 50 50 255 180))
    ~east_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 50 50 255 180))
    ~south_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 50 50 255 180))
    ~west_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 50 50 255 180))
    ~top_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 50 50 255 180))
    ~bottom_face:(fun () ->
      draw_plane
        (Vector3.create 0.0 0.5 0.0)
        (Vector2.create 1.0 1.0)
        (Color.create 50 50 255 180))
