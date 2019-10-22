# purehs.org

[![Build Status](https://travis-ci.org/grumply/purehs.org.svg?branch=master)](https://travis-ci.org/grumply/purehs.org)

This repository implements [purehs.org](http://www.purehs.org).

## Building

> ***Important!***
>
> Be sure to follow the instructions at [purehs.cachix.org](https://purehs.cachix.org) to prepare for installation. For a full development environment, follow the instructions for setting up an IDE at [purehs.org/tut/install](http://purehs.org/tut/install).

Download and install dependencies.

```bash
git clone --depth=1 https://github.com/grumply/purehs.org
cd purehs.org/
./ghcjs npm install
```

Build for production.

```bash
./ghc npm run prod:backend
./ghcjs npm run prod:frontend
```