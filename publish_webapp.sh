#!/bin/bash
set -e
cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")/crates/fanttheysia-webapp-frontend"
trunk build --release --public-url "fanttheysia/editor" --dist dist_prod
rsync --verbose --progress --stats --compress --recursive --times --perms --delete dist_prod/* nfsn:fanttheysia/editor/
