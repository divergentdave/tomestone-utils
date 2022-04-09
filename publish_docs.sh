#!/bin/bash
set -e
cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
cargo doc --workspace --no-deps
rsync --verbose --progress --stats --compress --recursive --times --perms --delete target/doc/* nfsn:fanttheysia/doc/
