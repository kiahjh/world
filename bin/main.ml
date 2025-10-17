let setup () =
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Warning;
  (* Suppress INFO messages *)
  Raylib.init_window 1600 1000 "raylib [core] example - 3d cube";
  Raylib.set_window_state [ Raylib.ConfigFlags.Window_resizable ];
  Raylib.set_target_fps 120;
  Raylib.set_exit_key Raylib.Key.Null (* Disable ESC from closing window *)

let rec loop shader pos_x pos_y pos_z angle_x angle_y =
  if Raylib.window_should_close () then (
    Raylib.unload_shader shader;
    Raylib.close_window ())
  else
    let open Raylib in
    let screen_width = get_screen_width () in
    let screen_height = get_screen_height () in
    let target = load_render_texture screen_width screen_height in
    (* Unlock with ESC, lock cursor on click *)
    if is_key_pressed Key.Escape then enable_cursor ();

    if
      is_mouse_button_pressed MouseButton.Left
      || is_mouse_button_pressed MouseButton.Right
    then disable_cursor ();

    (* Update camera rotation based on mouse *)
    let new_angle_x, new_angle_y =
      if is_cursor_hidden () then
        let delta = get_mouse_delta () in
        let unclamped_x = angle_x -. (Vector2.y delta *. 0.0015) in
        (* Clamp to slightly less than 90 degrees to avoid rendering issues *)
        let max_angle = (Float.pi /. 2.0) -. 0.01 in
        let clamped_x = max (-.max_angle) (min max_angle unclamped_x) in
        (clamped_x, angle_y -. (Vector2.x delta *. 0.0015))
      else (angle_x, angle_y)
    in

    (* Calculate look direction from angles *)
    let look_x = sin new_angle_y *. cos new_angle_x in
    let look_y = sin new_angle_x in
    let look_z = cos new_angle_y *. cos new_angle_x in

    (* Calculate horizontal movement direction (ignoring vertical angle) *)
    let forward_x = sin new_angle_y in
    let forward_z = cos new_angle_y in

    (* Calculate right vector for strafing (perpendicular to look direction) *)
    let right_x = cos new_angle_y in
    let right_z = -.sin new_angle_y in

    (* Handle WASD movement *)
    let base_speed = 0.1 in
    let move_speed =
      if is_mouse_button_down MouseButton.Right then base_speed *. 10.0
      else base_speed
    in
    let new_pos_x, new_pos_y, new_pos_z =
      let px = ref pos_x in
      let py = ref pos_y in
      let pz = ref pos_z in

      (* W - forward *)
      if is_key_down Key.W then (
        px := !px +. (forward_x *. move_speed);
        pz := !pz +. (forward_z *. move_speed));

      (* S - backward *)
      if is_key_down Key.S then (
        px := !px -. (forward_x *. move_speed);
        pz := !pz -. (forward_z *. move_speed));

      (* A - strafe left *)
      if is_key_down Key.A then (
        px := !px +. (right_x *. move_speed);
        pz := !pz +. (right_z *. move_speed));

      (* D - strafe right *)
      if is_key_down Key.D then (
        px := !px -. (right_x *. move_speed);
        pz := !pz -. (right_z *. move_speed));

      (* Left Shift - move up *)
      if is_key_down Key.Left_shift then py := !py +. move_speed;

      (* Left Command - move down *)
      if is_key_down Key.Left_super then py := !py -. move_speed;

      (!px, !py, !pz)
    in

    (* Camera looks from position toward position + look direction *)
    let camera =
      Camera3D.create
        (Vector3.create new_pos_x new_pos_y new_pos_z)
        (Vector3.create (new_pos_x +. look_x) (new_pos_y +. look_y)
           (new_pos_z +. look_z))
        (Vector3.create 0.0 1.0 0.0)
        45.0 CameraProjection.Perspective
    in

    (* Render to texture with the 3D scene *)
    begin_texture_mode target;
    clear_background (Color.create 83 166 255 255);

    begin_mode_3d camera;

    Terrain.draw_terrain 100 0.5;

    (* top side of water surface *)
    draw_plane (Vector3.zero ())
      (Vector2.create 100.0 100.0)
      (Color.create 0 120 235 170);

    (* bottom side of water surface *)
    Rlgl.push_matrix ();
    Rlgl.rotatef 180.0 0.0 0.0 0.1;
    draw_plane (Vector3.zero ())
      (Vector2.create 100.0 100.0)
      (Color.create 0 120 235 170);
    Rlgl.pop_matrix ();

    end_mode_3d ();

    end_texture_mode ();

    (* Draw the render texture with shader applied *)
    begin_drawing ();

    begin_shader_mode shader;
    draw_texture_rec
      (RenderTexture.texture target)
      (Rectangle.create 0.0 0.0
         (float_of_int (RenderTexture.texture target |> Texture.width))
         (float_of_int (-(RenderTexture.texture target |> Texture.height))))
      (Vector2.create 0.0 0.0) Color.white;
    end_shader_mode ();

    draw_fps 10 10;
    draw_text "+x" 10 40 20 Color.red;
    draw_text "+y" 40 40 20 Color.green;
    draw_text "+z" 70 40 20 Color.blue;
    draw_text (string_of_float pos_x) 10 70 20 Color.red;
    draw_text (string_of_float pos_y) 10 100 20 Color.green;
    draw_text (string_of_float pos_z) 10 130 20 Color.blue;

    unload_render_texture target;
    end_drawing ();
    loop shader new_pos_x new_pos_y new_pos_z new_angle_x new_angle_y

let () =
  setup ();
  let shader = Raylib.load_shader "" "bin/shaders/pixelizer.fs" in
  (* Starting position: 10 units back, 5 units up *)
  let initial_pos_x = 0.0 in
  let initial_pos_y = 5.0 in
  let initial_pos_z = 10.0 in
  (* Starting look angles: looking slightly down and toward origin (180 degrees around) *)
  let initial_angle_x = -0.3 in
  let initial_angle_y = Float.pi in
  loop shader initial_pos_x initial_pos_y initial_pos_z initial_angle_x
    initial_angle_y
