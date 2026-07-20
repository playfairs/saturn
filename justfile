set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

default:
    @just --list

build:
    cabal build all

run:
    cabal run

test:
    cabal test all

lint:
    hlint .

clean:
    cabal clean
    rm -rf dist-newstyle