#!/bin/bash

set -e

LD_PRELOAD=~/Projects/mvsqlite/mvsqlite-preload/libmvsqlite_preload.so LD_LIBRARY_PATH=~/workspace/sqlite/sqlite-amalgamation-3390300 \
    RUST_LOG=warn \
    MVSQLITE_DATA_PLANE=http://localhost:7000 \
    GLUON_DB=gluon-dev \
    FLY_MACHINE_API_HOSTNAME=localhost \
    FLY_MACHINE_API_PORT=4280 \
    FLY_MACHINE_API_TOKEN=$(cat ~/.fly/token) \
    FLY_MACHINE_APP_NAME=boson-test \
    stack exec gluon-statekeeper-exe
