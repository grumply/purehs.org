----------------
title: Installation
author: sean
highlights: []
----------------

Pure can be installed with [Cabal](https://www.haskell.org/cabal/), [Stack](https://haskellstack.org), or [Nix](https://nixos.org). The easiest way to get started with Pure is via a Nix environment.

> # Note:
> Pure is not yet available through [Hackage](https://hackage.haskell.org), [Stackage](https://www.stackage.org), or [nixpkgs](https://github.com/NixOS/nixpkgs).

## Nix

[Nix](https://nixos.org/nix/) is able to manage the core dependencies for a Pure development environment that can be shared across projects, including Haskell compilers, GHC and GHCJS; Cabal; node; and a couple of utility libraries.

Once you've installed Nix, the following bash command will start a localized shell with the dependencies available:

```bash
nix-shell -p haskell.compiler.ghcjsHEAD -p cabal-install -p zlib -p libiconv -p nodePackages.npm -p nodejs
```

> # Note:
> The first call to `nix-shell` will be slow, but all subsequent calls will be near-instantaneous. For the first install, you may want to grab a cup of coffee.

## Dependencies

At a minimum, [tlc](https://github.com/grumply/tlc), [ef](https://github.com/grumply/ef), [ef-base](https://github.com/grumply/ef-base), and [pure](https://github.com/grumply/pure) will be required. Optionally, you may also install [trivial](https://github.com/grumply/trivial) for easy benchmarking and testing, and [purify](https://github.com/grumply/purify) for project initialization and automated scaffolding tools.

As a simple one-liner:

```bash
for i in {tlc,ef,ef-base,pure}; do git clone https://github.com/grumply/$i.git; cabal install $i/; cabal install --ghcjs $i/; done
```

or with [trivial](https://github.com/grumply/trivial) and [purify](https://github.com/grumply/purify):

```bash
git clone https://github.com/grumply/purify.git && cabal install purify/
for i in {tlc,trivial,ef,ef-base,pure}; do git clone https://github.com/grumply/$i.git; cabal install $i/; cabal install --ghcjs $i/; done
```
