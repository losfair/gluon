#!/bin/bash

set -e

export GLUON_DB=gluon-dev
export LD_PRELOAD=/home/ubuntu/Projects/mvsqlite/mvsqlite-preload/libmvsqlite_preload.so 
export RUST_LOG=info
export MVSQLITE_DATA_PLANE=http://localhost:7000
export LD_LIBRARY_PATH=/home/ubuntu/workspace/sqlite/sqlite-amalgamation-3390300

export FLY_MACHINE_API_ORIGIN=http://localhost:4280
export FLY_MACHINE_API_TOKEN=$(cat ~/.fly/token)
export FLY_MACHINE_APP_NAME=boson-test

npx next dev
