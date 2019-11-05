# Hilda

Download the project, e.g.:

    git clone https://.../hilda.git
    cd hilda

Configure the project:

    make setup

Build it:

    make build

Run it:

    cabal new-run hilda

With arguments:

    cabal new-run hilda -- foo bar --help

Change something:

    vim src/Main.hs
    ...

Rebuild:

    make build

Or just re-run:

    cabal new-run hilda ...

