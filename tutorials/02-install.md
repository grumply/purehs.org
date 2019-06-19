# Getting And Running Pure

To get started, clone the project skeleton and install dependencies. 

```bash
$ git clone https://github.com/grumply/pure-project-skeleton myapp/
$ cd myapp/
$ ./ghcjs npm install
```

Run a development web server serving frontend client. 

```bash
$ ./ghcjs npm run dev:frontend
```

In another shell, run the backend server.

```bash
$ ./ghc npm run dev:backend
```

These servers will watch source files for changes and recompile as necessary.
