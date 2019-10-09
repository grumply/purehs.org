# Installation

To get started, follow the instructions at [purehs.cachix.org](https://purehs.cachix.org).

Next, clone the project skeleton and install dependencies. 

```bash
git clone --depth=1 https://github.com/grumply/pure-project-skeleton myapp/
cd myapp/
rm -rf ./.git
./ghcjs npm install
```

Run a development web server serving the frontend client. 

```bash
./ghcjs npm run dev:frontend
```

In another shell, run the backend server.

```bash
./ghc npm run dev:backend
```

These development servers will watch source files for changes and recompile as necessary.

## IDE

The `pure-project-skeleton` includes a simple wrapper for easy VS Code + Haskell IDE Engine (hie) integration.

Enable the all-hies cachix cache:

```bash
cachix use all-hies
```

Install the ghc 8.4.4 hie to match pure-platform's ghc version:

> This installation is sytem-wide and needs be performed only once.

```bash
nix-env -iA selection --arg selector 'p: { inherit (p) ghc844; }' -f https://github.com/infinisil/all-hies/tarball/master
```

Finally, in VS Code settings search for `useHieWrapper` and be sure the `Use Custom Hie Wrapper` checkbox is selected and set the `User Custom Hie Wrapper Path` to:

```bash
${workspaceFolder}/lsp
```

## Tutorials

For a quick start, check out the starter [tutorials](/tut/basics).

## Join the Community

If you've got questions or want to help answer questions, check out the [discourse](http://discourse.purehs.org).

