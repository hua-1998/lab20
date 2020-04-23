open Graphics;;

class image (orig : (float list) list) =
  object(this)
    val orig = orig

    method depict (img : float list list): unit =
      Graphics.open_graph "";
      Graphics.clear_graph ();
      let x_dim, y_dim = List.length (List.hd img), List.length img
      in Graphics.resize_window x_dim y_dim;
      let depict_pix (v : float) (r_pos : int) (c_pos : int) : unit =
        let lvl = int_of_float (255. *. (1. -. v))
        in Graphics.set_color (Graphics.rgb lvl lvl lvl);
        plot c_pos (y_dim - r_pos)
      in List.iteri (fun r row ->
          List.iteri (fun c pix -> depict_pix pix r c) row) img;
      Unix.sleep 2; Graphics.close_graph ()

    method original : unit = this#depict orig

    method threshold (threshold_num : float) : unit =
      let threshold_img =
        List.map (fun row ->
            List.map (fun v -> if v <= threshold_num then 0. else 1.) row) orig
      in this#depict threshold_img

    method dither : unit =
      let dither_img =
        List.map (fun row ->
          List.map (fun v -> if v > Random.float 1. then 1. else 0.) row) orig
      in this#depict dither_img
  end ;;

let mona = new image Monalisa.image ;;
mona#original ;;
mona#threshold 0.75;;
mona#dither ;;
