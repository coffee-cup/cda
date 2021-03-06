# cda

Command line app for creating and managing `cd` aliases.

![cda](https://user-images.githubusercontent.com/3044853/32304787-e4a5b214-bf2e-11e7-9e29-8a9dc0c5a063.gif)

## Installation

This tool can be installed as a [Homebrew](https://brew.sh/) [tap](https://github.com/Homebrew/brew/blob/master/docs/brew-tap.md)

```
brew tap coffee-cup/cda
brew install cda
```

Run `cda init` and see usage instructions.

## Usage

```
❯ cda -h
Usage: cda COMMAND
  Create and manage cd aliases

Available options:
  -h,--help                Show this help text

Available commands:
  list                     List cd aliases
  init                     Initializes alias file
  set                      Create cd alias
  rm                       Remove cd alias
```

### Init

Initializes the aliases with

```
cda init
```

Then place

```sh
export -f cda () { command cda "$@"; source ~/.cda; }; source ~/.cda;
```

in your `~/.bash_profile` or `~/.zshrc` or equivalent. This bash function just sources the alias file after each run of `cda` and on startup of your shell.

You should then source your config (`~/.bash_profile` or `~/.zshrc`) to load this function.

### Create Alias

Create and alias called `NAME` which points to `DIRECTORY`

```
cda set NAME DIRECTORY
```

For example, create an alias called `p` which will `cd` you into the `~/dev/project` directory.

```
~/dev/project
❯ cda set p .
Created alias p

~/dev/project
❯ cda list
p -> /Users/user/dev/project
```

You can then use the alias with the `p` command.

### Remove Alias

```
cda rm NAME
```

For example,

```
cda rm d
```

## Development

Use [Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack build
stack exec cda COMMAND
```
