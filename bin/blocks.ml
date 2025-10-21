open Raylib
open Raylib.Rlgl

let draw_stone_block ~x ~y ~z ~north ~east ~south ~west ~top ~bottom =
  let xf = float_of_int x in
  let yf = float_of_int y in
  let zf = float_of_int z in
  if north then (
    push_matrix ();
    translatef xf yf zf;
    rotatef (-90.0) 0.0 0.0 1.0;
    draw_plane (Vector3.create 0.0 0.5 0.0) (Vector2.create 1.0 1.0) Color.gray;
    pop_matrix ());
  if east then (
    push_matrix ();
    translatef xf yf zf;
    rotatef 90.0 1.0 0.0 0.0;
    draw_plane (Vector3.create 0.0 0.5 0.0) (Vector2.create 1.0 1.0) Color.gray;
    pop_matrix ());
  if south then (
    push_matrix ();
    translatef xf yf zf;
    rotatef 90.0 0.0 0.0 1.0;
    draw_plane (Vector3.create 0.0 0.5 0.0) (Vector2.create 1.0 1.0) Color.gray;
    pop_matrix ());
  if west then (
    push_matrix ();
    translatef xf yf zf;
    rotatef (-90.0) 1.0 0.0 0.0;
    draw_plane (Vector3.create 0.0 0.5 0.0) (Vector2.create 1.0 1.0) Color.gray;
    pop_matrix ());
  if top then (
    push_matrix ();
    translatef xf yf zf;
    draw_plane (Vector3.create 0.0 0.5 0.0) (Vector2.create 1.0 1.0) Color.gray;
    pop_matrix ());
  if bottom then (
    push_matrix ();
    translatef xf yf zf;
    rotatef 180.0 1.0 0.0 0.0;
    draw_plane (Vector3.create 0.0 0.5 0.0) (Vector2.create 1.0 1.0) Color.gray;
    pop_matrix ())
