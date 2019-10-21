{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/9a682a8d9e3b28f37015c6350d6d196375c70085) {};

in pure-platform.project ({ pkgs, ... }: {

  packages = {
    backend = ./backend;
    frontend = ./frontend;
    shared = ./shared;
  };

  shells = {
    ghc = [ "frontend" "backend" "shared" ];
    ghcjs = [ "frontend" "shared" ];
  };

})


