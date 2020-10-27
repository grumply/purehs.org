**Average installation time: A few minutes _(pre-built binary downloads)_**

The new [Haskell VSCode extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) works exceptionally well. Just be sure to start the editor from a project-associated nix shell.

```bash
nix-shell default.nix -A shells.ghc
code .
```

That's it, you should see type signatures, completions, and other goodies. 

> Tip: Use ⌘ or ⎈ (control) to get more information on hover.