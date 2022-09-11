#!/bin/bash

set -e

export BOSON_DB=boson-dev
export LD_PRELOAD=/home/ubuntu/Projects/mvsqlite/mvsqlite-preload/libmvsqlite_preload.so 
export RUST_LOG=info
export MVSQLITE_DATA_PLANE=http://localhost:7000
export LD_LIBRARY_PATH=/home/ubuntu/workspace/sqlite/sqlite-amalgamation-3390300

npx next dev
