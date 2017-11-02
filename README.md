# cda

Command line app for creating and managing `cd` aliases.

![cda](https://user-images.githubusercontent.com/3044853/32304742-af38a3ca-bf2e-11e7-94e7-0daa3c85310f.gif)

## Usage

```
❯ cda -- -h
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
export -f cda () { command cda "$@"; source ~/.cda; }
```

in your `~/.bash_profile` or `~/.zshrc` or equivalent. This bash function just sources the alias file after each run of `cda`.

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

You can then use the alias with the `d` command.

### Removing Aliases

Remove an alias.

```
cda rm NAME
```

For example,

```
cda rm d
```
