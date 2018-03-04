{}:

(import ./deps/pure-platform {}).project ({ pkgs, ... }: {
  overrides = self: super: {
    hslua-module-text = self.callHackage "hslua-module-text" "0.1.2.1" {};
    hslua = self.callHackage "hslua" "0.9.5.1" {};
    pandoc = self.callHackage "pandoc" "2.1.1" {};
    doctemplates = self.callHackage "doctemplates" "0.2.1" {};
    pandoc-types = self.callHackage "pandoc-types" "1.17.3.1" {};
    skylighting = self.callHackage "skylighting" "0.5.1" {};
    tagsoup = self.callHackage "tagsoup" "0.14.6" {};
    texmath = self.callHackage "texmath" "0.10.1.1" {};
    ansi-terminal = self.callHackage "ansi-terminal" "0.6.3.1" {};
  };

  packages = {
    excelsior = ./deps/excelsior;
    semantic-ui-pure = ./deps/semantic-ui-pure;
    pure-tagsoup = ./deps/pure-tagsoup;
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
