{inputs, ...}: rec {
  luaPath = ./.;
  defaultPackageName = "nvimFull";
  packageNames = [defaultPackageName "nvim"];
  categoryDefinitions = {
    pkgs,
    settings,
    categories,
    extra,
    name,
    mkPlugin,
    ...
  } @ packageDef: {
    # to define and use a new category, simply add a new list to a set here,
    # and later, you will include categoryname = true; in the set you
    # provide when you build the package using this builder function.
    # see :help nixCats.flake.outputs.packageDefinitions for info on that section.

    # lspsAndRuntimeDeps:
    # this section is for dependencies that should be available
    # at RUN TIME for plugins. Will be available to PATH within neovim terminal
    # this includes LSPs
    lspsAndRuntimeDeps = {
      general = with pkgs; [
        ripgrep
      ];

      devel = with pkgs; [
        # c
        clang-tools
        gdb
        # rust
        rust-analyzer
        clippy
        # haskell
        haskell-language-server
        # python
        basedpyright
        # nix
        nixd
        alejandra
        # lua
        lua-language-server
        stylua
      ];

      edu = with pkgs; [
        # typst
        unstable.tinymist
      ];
    };

    # This is for plugins that will load at startup without using packadd:
    startupPlugins = {
      general = with pkgs.vimPlugins; [
        nvim-highlight-colors
        base16-nvim
        bufferline-nvim
        toggleterm-nvim
        smart-splits-nvim

        oil-nvim
        mini-align
        mini-pick
        mini-icons
      ];

      devel = with pkgs.vimPlugins; [
        nvim-lspconfig
        nvim-treesitter.withAllGrammars
        comment-nvim
      ];

      edu = with pkgs; [
        unstable.vimPlugins.typst-preview-nvim
      ];
    };

    # not loaded automatically at startup.
    # use with packadd and an autocommand in config to achieve lazy loading
    optionalPlugins = {
      # TODO: figure out what to do with this
      general = with pkgs.vimPlugins; [
      ];
    };

    # shared libraries to be added to LD_LIBRARY_PATH
    # variable available to nvim runtime
    sharedLibraries = {};
    environmentVariables = {};

    # categories of the function you would have passed to withPackages
    python3.libraries = {
      # test = [ (_:[]) ];
    };

    # If you know what these are, you can provide custom ones by category here.
    # If you dont, check this link out:
    # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/setup-hooks/make-wrapper.sh
    extraWrapperArgs = {
      # test = [
      #   '' --set CATTESTVAR2 "It worked again!"''
      # ];
    };
  };

  # see :help nixCats.flake.outputs.packageDefinitions
  packageDefinitions = {
    # These are the names of your packages
    # you can include as many as you wish.
    nvim = {
      pkgs,
      name,
      ...
    }: {
      settings = {
        suffix-path = true;
        suffix-LD = true;
        wrapRc = true;
        aliases = []; # IMPORTANT: your alias may not conflict with your other packages.
        neovim-unwrapped = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
      };

      # and a set of categories that you want
      # (and other information to pass to lua)
      categories = {
        general = true;
      };
      # anything else to pass and grab in lua with `nixCats.extra`
      extra = {
        nixdExtras.nixpkgs = ''import ${pkgs.path} {}'';
      };
    };

    nvimFull = {
      pkgs,
      name,
      ...
    }: {
      # they contain a settings set defined above
      # see :help nixCats.flake.outputs.settings
      settings = {
        suffix-path = true;
        suffix-LD = true;
        wrapRc = true;
        aliases = ["nvim"]; # IMPORTANT: your alias may not conflict with your other packages.
        neovim-unwrapped = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
      };

      # and a set of categories that you want
      # (and other information to pass to lua)
      categories = {
        general = true;
        devel = true;
        edu = true;
      };
      # anything else to pass and grab in lua with `nixCats.extra`
      extra = {
        nixdExtras.nixpkgs = ''import ${pkgs.path} {}'';
      };
    };
  };
}
