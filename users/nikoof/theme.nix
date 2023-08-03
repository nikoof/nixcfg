{ pkgs, ... }:
{
  enable = true;

  gtk2.extraConfig = ''
    gtk-application-prefer-dark-theme = 1
    gtk-theme-name = "Nordic"
  '';

  theme = {
    name = "Graphite-Dark-nord";
    package = pkgs.graphite-gtk-theme; 
  };

  iconTheme = {
    name = "Nordzy";
    package = pkgs.nordzy-icon-theme;
  };

  cursorTheme = {
    name = "Simp1e-Nord-Dark";
    package = pkgs.simp1e-cursors;
  };

  font = {
    name = "Sans Regular";
    size = 11;
  };
}
