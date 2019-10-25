# Installation

**Average installation time: 10 minutes _(mostly pre-built binary downloads)_**

To get started, follow the instructions at [purehs.cachix.org](https://purehs.cachix.org).

Next, clone the project skeleton. 

```bash
git clone --depth=1 https://github.com/grumply/pure-project-skeleton myapp/
cd myapp/
rm -rf ./.git
```

Prepare the frontend development server using a nix shell `./ghcjs` with `npm`.

```bash
./ghcjs npm install
```

Run a development web server inside a nix shell serving the frontend client locally at [localhost:8080](localhost:8080) with a control panel at [localhost:3000](localhost:3000). 

```bash
./ghcjs npm run dev:frontend
```

In a separate shell, run the backend websocket server.

```bash
./ghc npm run dev:backend
```

These development servers will watch source files for changes and recompile as necessary. When the frontend changes, the frontend development server will reload any connected browser pages.

## IDE

**Average installation time: 8 minutes _(mostly pre-built binary downloads)_**

The `pure-project-skeleton` includes a simple wrapper for easy [VS Code](https://code.visualstudio.com) + [Haskell IDE Engine (hie)](https://github.com/haskell/haskell-ide-engine) integration to provide type signatures, completions, and other goodies.

Install the haskell-language-server VS Code plugin from [visualstudio.com](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) and ignore the build requirements.

Enable the all-hies cachix cache:

```bash
cachix use all-hies
```

Install a system-wide `hie` for `ghc-8.4.4`:

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

