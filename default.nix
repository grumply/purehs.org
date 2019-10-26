{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/master) {};

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


