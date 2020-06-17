**Average installation time: 10 minutes _(mostly pre-built binary downloads)_**

<div class="info">
This installation method is applicable to MacOS and Linux. Windows is not currently supported, sorry! If you're on Windows, grab a Linux VM with [VirualBox](https://www.virtualbox.org) before following these instructions.
</div>

To get started, set up the Pure.hs cachix cache via the instructions at [purehs.cachix.org](https://purehs.cachix.org).

## pure-platform

`pure-platform` is useful for writing quick one-off web interfaces, running a GHCi session, or testing a script. If you're going through the [5-Minute Series](/tutorials/5-minute), this is the installation method to follow. If you're building an application, you'll want the [pure-project-skeleton](#pure-project-skeleton), below.

First, clone the pure-platform repository.

```bash
git clone --depth=1 https://github.com/grumply/pure-platform
```

<div class="info">
If this is your first time installing the `pure-platform` or `pure-project-skeleton`, it may take a few minutes to download all of the necessary libraries. Subsequent runs will use a locally cached version, and take only a few seconds to get up and running.
</div>

And start up a `nix-shell`.

```bash
cd pure-platform
nix-shell
```

Now you have access to `ghcjs` with all of the Pure.hs libraries available. This is all you need to get started with the [5-Minute Series](/tutorials/5-minute).

## pure-project-skeleton

`pure-project-skeleton` is useful for writing a full client-server application. If you're going through the [5-Minute Series](/tutorials/5-minute), this is not the installation method to follow. If you're following the tutorials, you'll want the [pure-platform](#pure-platform), above.

First, clone the pure-project-skeleton repository. 

```bash
git clone --depth=1 https://github.com/grumply/pure-project-skeleton myapp/
```

<div class="info">
If this is your first time installing the `pure-platform` or `pure-project-skeleton`, it may take a few minutes to download all of the necessary libraries. Subsequent runs will use a locally cached version, and take only a few seconds to get up and running.
</div>

Run the frontend and backend development servers in separate shells. 

```bash
./develop --ghcjs
```

```bash
./develop --ghc
```

These development servers will watch source files for changes and recompile as necessary.  The frontend server will serve the application at [localhost](localhost). The backend server serves websocket requests at [localhost:8081](localhost:8081).

<div class="info">
The development system currently requires two seaparate invocations (`--ghc` and `--ghcjs`) for performance reasons.

Save a backend file after startup to have the development system invoke the backend server for the first time. (This should be fixed in a future update.)
</div>

## IDE

If you're looking to integrate pure-project-skeleton with an IDE, check out the [Visual Studio Code Integration Tutorial](./vscode) or the [Vim Integration Tutorial](./vim)

## Tutorials

For a quick start, check out the [5-Minute Tutorial Series](/tutorials/5-minute).

## Join the Community

If you've got questions or want to help answer questions, check out the [discourse](http://discourse.purehs.org).

