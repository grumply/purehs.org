# Getting And Running Pure

To get started, clone the project skeleton:

```bash
$ git clone https://github.com/grumply/pure-project-skeleton myapp/ --recurse-submodules
$ cd myapp/
```

If this is your first time running Pure, you'll have to run `try-pure` to install dependencies: 

```bash
$ ./deps/pure-platform/try-pure
```

Note that `try-pure` will take a very long time to run the first time. Since Pure uses [nix](https://nixos.org/nix/) to manage dependencies and memoize builds, subsequent runs of `try-pure` will not be slow. 

Now install node depenencies:

```bash
$ ./ghc npm install
```

And we can finally build and run a frontend development server:

```bash
$ ./ghcjs npm run dev:frontend
```

This server will watch `shared/` and `frontend/` and rebuild and reload the application as necessary when files change.

In a different shell, build and run the backend server:

```bash
$ ./ghc npm run dev:backend
```

This server will watch `shared/` and `backend/` and rebuild and reload the server as necessary when files change.
