#!/bin/bash

set -e

LD_PRELOAD=~/Projects/mvsqlite/mvsqlite-preload/libmvsqlite_preload.so LD_LIBRARY_PATH=~/workspace/sqlite/sqlite-amalgamation-3390300 \
    RUST_LOG=info \
    MVSQLITE_DATA_PLANE=http://localhost:7000 \
    BOSON_DB=boson-dev \
    FLY_MACHINE_API_HOSTNAME=localhost:4280 \
    FLY_MACHINE_API_TOKEN=ffff \
    stack exec boson-statekeeper-exe
