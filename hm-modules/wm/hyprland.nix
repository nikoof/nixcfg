{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: let
  cfg = config.wm.hyprland;
in {
  options.wm.hyprland = {
    enable = lib.mkEnableOption "Hyprland";

    wallpaper = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      swww

      cliphist
      wl-clipboard

      pamixer
      playerctl
    ];

    programs.rofi.enable = true;
    programs.rofi.package = pkgs.rofi-wayland;

    programs.waybar.enable = true;
    programs.waybar.settings = {
      mainBar = {
        layer = "top";
        position = "top";
        output = [
          "eDP-1"
        ];

        modules-left = ["hyprland/workspaces" "hyprland/submap"];
        modules-center = ["hyprland/window"];
        modules-right = ["battery" "clock"];

        "sway/window" = {
          max-length = 50;
        };

        battery = {
          format = "{capacity}% {icon}";
          format-icons = ["" "" "" "" ""];
        };

        clock = {
          format-alt = "{:%a, %d. %b  %H:%M}";
        };
      };
    };
    programs.waybar.style = ''
      * {
        border: none;
        border-radius: 0;
        font-family: FiraCode Nerd Font Mono;
      }
      window#waybar {
        background: #16191C;
        color: #AAB2BF;
      }
      #workspaces button {
        padding: 0 5px;
      }
    '';

    wayland.windowManager.hyprland.enable = true;
    wayland.windowManager.hyprland.settings = {
      "$mod" = "SUPER";

      monitor = [
        "eDP-1,preferred,auto,1"
      ];

      gestures = {
        workspace_swipe = true;
        workspace_swipe_fingers = 3;
      };

      exec-once = [
        "${pkgs.kdePackages.polkit-kde-agent-1}/usr/lib/polkit-kde-authentication-agent-1"
        "swww-daemon"
        "swww img ${cfg.wallpaper}"
        "waybar"
      ];

      bind =
        [
          "$mod, Return, exec, alacritty"
          "$mod, R, exec, rofi -show drun -show-icons"
          "$mod, B, exec, firefox"

          # Clipboard
          "$mod, V, exec, cliphist list | rofi -dmenu | cliphist decode | wl-copy"

          # Window management
          "$mod, W, killactive"
          "$mod, F, fullscreen"
          "$mod SHIFT, F, togglefloating"

          # Focus
          "$mod, H, movefocus, l"
          "$mod, J, movefocus, d"
          "$mod, K, movefocus, u"
          "$mod, L, movefocus, r"
          "$mod SHIFT, H, movefocus, l"
          "$mod SHIFT, J, movefocus, d"
          "$mod SHIFT, K, movefocus, u"
          "$mod SHIFT, L, movefocus, r"

          # Media
          ", XF86AudioRaiseVolume, exec, pamixer -i 5"
          ", XF86AudioLowerVolume, exec, pamixer -d 5"
          ", XF86AudioMicMute, exec, pamixer --default-source -m"
          ", XF86AudioMute, exec, pamixer -t"
          ", XF86AudioPlay, exec, playerctl play-pause"
          ", XF86AudioPause, exec, playerctl play-pause"
          ", XF86AudioStop, exec, playerctl stop"
          ", XF86AudioNext, exec, playerctl next"
          ", XF86AudioPrev, exec, playerctl previous"

          "$mod SHIFT, Q, exit"
        ]
        ++ (
          builtins.concatLists (builtins.genList (
              x: let
                workspace = let
                  c = (x + 1) / 10;
                in
                  builtins.toString (x + 1 - (c * 10));
              in [
                "$mod, ${workspace}, workspace, ${builtins.toString (x + 1)}"
                "$mod SHIFT, ${workspace}, movetoworkspacesilent, ${builtins.toString (x + 1)}"
              ]
            )
            10)
        );

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      decoration = {
        blur = {
          enabled = true;
          size = 3;
          passes = 3;
          new_optimizations = true;
          ignore_opacity = true;
        };

        drop_shadow = true;
        shadow_ignore_window = true;
        shadow_offset = "2 2";
        shadow_range = 4;
        shadow_render_power = 2;
        "col.shadow" = "rgba(00000066)";
      };

      general = with config.colorScheme.palette; {
        "col.active_border" = "rgba(${base0E}ff) rgba(${base09}ff) 60deg";
        "col.inactive_border" = "rgba(${base00}ff)";
      };

      misc = {
        enable_swallow = true;
        swallow_regex = "^(Alacritty)$";
      };

      xwayland = {
        force_zero_scaling = true;
      };

      env = [
        "GDK_BACKEND, wayland"
        "QT_QPA_PLATFORM, wayland"
        "SDL_VIDEODRIVER, wayland"
        "CLUTTER_BACKEND, wayland"
        "NIXOS_OZONE_WL, 1"
      ];
    };
  };
}
