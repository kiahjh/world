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

let draw_stone_block ~x ~y ~z ~north_exposed ~east_exposed ~south_exposed
    ~west_exposed ~top_exposed ~bottom_exposed ~user_x ~user_y ~user_z =
  let xf = float_of_int x in
  let yf = float_of_int y in
  let zf = float_of_int z in
  if north_exposed && user_x > xf then (
    push_matrix ();
    translatef xf yf zf;
    rotatef (-90.0) 0.0 0.0 1.0;
    draw_plane
      (Vector3.create 0.0 0.5 0.0)
      (Vector2.create 1.0 1.0)
      (Color.create 100 100 100 255);
    pop_matrix ());
  if east_exposed && user_z > zf then (
    push_matrix ();
    translatef xf yf zf;
    rotatef 90.0 1.0 0.0 0.0;
    draw_plane
      (Vector3.create 0.0 0.5 0.0)
      (Vector2.create 1.0 1.0)
      (Color.create 100 100 100 255);
    pop_matrix ());
  if south_exposed && user_x < xf then (
    push_matrix ();
    translatef xf yf zf;
    rotatef 90.0 0.0 0.0 1.0;
    draw_plane
      (Vector3.create 0.0 0.5 0.0)
      (Vector2.create 1.0 1.0)
      (Color.create 100 100 100 255);
    pop_matrix ());
  if west_exposed && user_z < zf then (
    push_matrix ();
    translatef xf yf zf;
    rotatef (-90.0) 1.0 0.0 0.0;
    draw_plane
      (Vector3.create 0.0 0.5 0.0)
      (Vector2.create 1.0 1.0)
      (Color.create 100 100 100 255);
    pop_matrix ());
  if top_exposed && user_y > yf then (
    push_matrix ();
    translatef xf yf zf;
    draw_plane
      (Vector3.create 0.0 0.5 0.0)
      (Vector2.create 1.0 1.0)
      (Color.create 110 110 110 255);
    pop_matrix ());
  if bottom_exposed && user_y < yf then (
    push_matrix ();
    translatef xf yf zf;
    rotatef 180.0 1.0 0.0 0.0;
    draw_plane
      (Vector3.create 0.0 0.5 0.0)
      (Vector2.create 1.0 1.0)
      (Color.create 100 100 100 255);
    pop_matrix ())
