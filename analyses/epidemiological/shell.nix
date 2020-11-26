let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c92939889627636e62f7c8c01d29b2a2e462f56f.tar.gz") {};
in
  pkgs.mkShell {
    name = "uk-intros-env";
    buildInputs = with pkgs; [
      R
      rPackages.dplyr
      rPackages.reshape2
      rPackages.ggplot2
      rPackages.future
      rPackages.furrr
      emacs
    ];
   shellHook = ''
             printf "\n\nWelcome to uk-intros-env\n\n"
      '';
  }
