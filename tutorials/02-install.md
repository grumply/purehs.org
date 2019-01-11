## Getting And Running Pure

Pure uses [nix](https://nixos.org/nix/) to set up a development environment. 

### Hello, World!

If this is your first time using Pure, know that the first build can take a while. Subsequent builds will be much faster.

First, clone [pure-platform](https://github.com/grumply/pure-platform):

```bash
git clone https://github.com/grumply/pure-platform
cd pure-platform
```

Now, build the necessary development tools and drop into a shell with access to them:

```bash
./try-pure
```

Now create a file `Main.hs` with these contents:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Pure

main = inject body "Hello, World!"
```

Build the application:

```bash
ghcjs --make Main.hs
```

Run the result:

```bash
open Main.jsexe/index.html
```
