{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.apps.sioyek;
  unbinds = cmds:
    builtins.listToAttrs
    (map (cmd: {
        name = cmd;
        value = "<unbound>";
      })
      cmds);
in {
  options = {
    apps.sioyek.enable = lib.mkEnableOption "Sioyek";
  };

  config = lib.mkIf cfg.enable {
    programs.sioyek = {
      enable = true;
      config = {
        startup_commands = "toggle_visual_scroll";
        should_launch_new_window = "1";
        should_warn_about_user_key_override = "0";
        create_table_of_contents_if_not_exists = "1";
        max_created_toc_size = "5000";

        ui_font = "Fira Code";
        show_doc_path = "1";
        show_closest_bookmark_in_statusbar = "1";
        default_dark_mode = "1";

        prerender_next_page_presentation = "1";
        super_fast_search = "1";
      };

      bindings =
        {
          # General
          open_prev_doc = "P";
          open_document_embedded = "o";
          open_document_embedded_from_current_path = "O";
          toggle_dark_mode = "<C-r>";
          copy = "y";

          # Navigation
          add_highlight = "H";

          move_left = "l";
          move_down = "j";
          move_up = "k";
          move_right = "h";

          move_visual_mark_down = "j";
          move_visual_mark_up = "k";

          next_page = "J";
          previous_page = "K";

          prev_state = "<C-o>";
          next_state = "<C-i>";

          goto_toc = "<tab>";

          keyboard_select = "v";
          toggle_visual_scroll = "V";

          # Zoom
          zoom_in = "+";
          zoom_out = "-";
          fit_to_page_width = "<C-<backspace>>";
          fit_to_page_width_smart = "<C-S-<backspace>>";
        }
        // unbinds [
          "open_document"
        ];
    };
  };
}
