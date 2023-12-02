default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid --target=advent-of-code-solutions -W --run=":! ghcid -c \"cabal repl exe:advent-of-code\" --warnings -T :main"

test:
    ghcid --target=advent-of-code-solutions -W --run=":! ghcid --target=tests --run"
