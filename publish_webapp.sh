#!/bin/bash
set -e
cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")/crates/fanttheysia-webapp-frontend"
trunk build --release --public-url "fanttheysia/editor"
rsync --verbose --progress --stats --compress --recursive --times --perms --delete dist/* nfsn:fanttheysia/editor/
