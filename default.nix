{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/23e0dc3df364da46979789b208b8ce8b8b3a71f6) {};

in pure-platform.project ({ pkgs, ... }: {

  minimal = true;

  packages = {
    backend = ./app/backend;
    shared = ./app/shared;
    frontend = ./app/frontend;
    test = ./app/test;

    server = ./dev/server;
    dev = ./dev/dev;
  };

  shells = {
    ghc = [ "dev" "server" "backend" "shared" "frontend" "test" ];
    ghcjs = [ "shared" "frontend" ];
  };

  tools = ghc: with ghc; [
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.fsnotify
    pkgs.pkgs.dhall-json
  ];

})