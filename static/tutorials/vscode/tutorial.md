**Average installation time: 8 minutes _(mostly pre-built binary downloads)_**

> Note this approach will only be compatible with Pure.hs-based projects. But you may choose to use this method to start non-pure haskell projects, as well, to retain `lsp` functionality.

The `pure-project-skeleton` includes a simple wrapper for easy [VS Code](https://code.visualstudio.com) integration with [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine) to provide type signatures, completions, and other goodies.

First, enable the all-hies cachix cache and install `hie` for `ghc-8.6.5`:

```bash
cachix use all-hies
nix-env -iA selection --arg selector 'p: { inherit (p) ghc865; }' -f https://github.com/infinisil/all-hies/tarball/master
```

Install the haskell-language-server VS Code plugin from [visualstudio.com](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) and ignore the build requirements.

Finally, in VS Code settings search for `useHieWrapper` and be sure the `Use Custom Hie Wrapper` checkbox is selected and set the `User Custom Hie Wrapper Path` to:

```bash
${workspaceFolder}/develop
```

That's it, you should be ready to fire up a Pure.hs project and see type signatures, completions, and other goodies. 

> Use ⌘ or ⎈ (control) to get more information on hover.