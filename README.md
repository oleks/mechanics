## Reproduce my Development Environment, Reproduce my Results!

The primary entry-points for working with this apparatus are the respective
`run-*.sh` shell scripts. These employ [Docker](https://www.docker.com) to run
the [Haskell](run-haskell.sh), or [Idris](run-idris.sh) apparatus,
respectively.  This bears the benefits of (1) a minimal-effort install, and (2)
a reproducible development environment.

To get started, install Docker, and run the respective shell script to start up
a Docker image with the respective tools (GHC and/or Idris) installed.

## Gotcha's

### The Idris Interpreter is Lazy

The Idris interpreter is lazy, while the Idris run-time is eager, unless
specified otherwise. This makes the interpreter useless for the shotgun testing
of run-time behaviour. Things that terminate in the interpreter, might not
terminate at run-time.
