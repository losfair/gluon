#!/bin/bash

set -e

cd "$(dirname $0)"

export PATH="$PWD/scripts/bin:$PATH"
export EXTRA_INCLUDE_PATH="/home/ubuntu/workspace/sqlite/sqlite-amalgamation-3390300"

stack build
