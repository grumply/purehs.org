# Installation

**Average installation time: 10 minutes _(mostly pre-built binary downloads)_**

To get started, follow the instructions at [purehs.cachix.org](https://purehs.cachix.org).

Next, clone the project skeleton. 

```bash
git clone --depth=1 https://github.com/grumply/pure-project-skeleton myapp/
```

Run the frontend and backend development servers in separate shells.

```bash
./develop --ghcjs
```

```bash
./develop --ghc
```

These development servers will watch source files for changes and recompile as necessary.  The frontend server will serve the application at [localhost](localhost). The backend server serves websocket requests at [localhost:8081](localhost:8081).

> ## Note 
> 
> The development system currently requires two seaparate invocations (`--ghc` and `--ghcjs`) for performance reasons.

## IDE

**Average installation time: 8 minutes _(mostly pre-built binary downloads)_**

The `pure-project-skeleton` includes a simple wrapper for easy [neovim](https://neovim.io) or [VS Code](https://code.visualstudio.com) integration with [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine) to provide type signatures, completions, and other goodies.

First, enable the all-hies cachix cache and install `hie` for `ghc-8.6.5`:

```bash
cachix use all-hies
nix-env -iA selection --arg selector 'p: { inherit (p) ghc865; }' -f https://github.com/infinisil/all-hies/tarball/master
```

### Vim

> Note this approach will only be compatible with pure-based projects. But you may choose to use this method to start non-pure haskell projects, as well, to retain `lsp` functionality. If you want to consider using this alongside another configuration, a good approach might be [GNU Stow](https://www.gnu.org/software/stow/).

Use your preferred method to install [coc.nvim](https://github.com/neoclide/coc.nvim). 

Find your `coc-settings.json` configuration file as per the instructions [here](https://github.com/neoclide/coc.nvim/wiki/Using-the-configuration-file#configuration-file-resolve) and add a haskell language server:

```json
{
  "languageserver": {
    "haskell": {
      "command": "./lsp",
      "rootPatterns": [ "cabal.project" ],
      "filetypes": [ "hs", "lhs" ],
      "initializationOptions": {
        "languageServerHaskell": {
        }
      }
    }
  }
}
```

### VS-Code

> Note this approach will only be compatible with pure-based projects. But you may choose to use this method to start non-pure haskell projects, as well, to retain `lsp` functionality.

Install the haskell-language-server VS Code plugin from [visualstudio.com](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) and ignore the build requirements.

Finally, in VS Code settings search for `useHieWrapper` and be sure the `Use Custom Hie Wrapper` checkbox is selected and set the `User Custom Hie Wrapper Path` to:

```bash
${workspaceFolder}/lsp
```

## Tutorials

For a quick start, check out the starter [tutorials](/tut/basics).

## Join the Community

If you've got questions or want to help answer questions, check out the [discourse](http://discourse.purehs.org).

