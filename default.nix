{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/03b379db0295f72e5c2dc80af0edaad8f9a1f9fa) {};

in pure-platform.project ({ pkgs, ... }: {

  minimal = true;

  packages = {
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = [ "frontend" "backend" ];
    ghcjs = [ "frontend" ];
  };

})