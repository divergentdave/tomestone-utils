# Tomestone Utilities
## Overview
This repository has libraries and programs for datamining and (eventually) modding Final Fantasy XIV. The primary focus is on text in the EXDF files, inside the game's data packs.

## Installation
Pre-compiled binaries are not yet provided.

To build from source, clone this repository, install a rust toolchain using [rustup](https://www.rustup.rs/), and then run `cargo build` or `cargo build --release`.

## Provide FFXIV's Installation Directory
The programs will expect FFXIV's installation directory to be provided either in a `.env` file, or in the environment variable FFXIV_INSTALL_DIR. If you installed FFXIV to the default location on an English-language 64-bit Windows OS, you can simply copy and use the example file as follows.

```
copy .env.example .env
```

If FFXIV is installed in a different location, copy the file as above, and then edit `.env` to provide the correct path.

## Usage
As of now, the main entry point is the `tomestone-dump` binary. Compile and run it as follows.

```
cargo run --release --bin tomestone-dump -- help
```

### tomestone-dump discover_paths
This subcommand scans every file for possible path names of other files, and stores them in a local database for future use. The data packs do not store compressed files by file name, rather, they use the CRC-32 checksum of the file name. Run the following command once, wait a few minutes for it to finish, and in the future, the subcommands `list` and `grep` will print filenames when possible. (Currently, this can recover the filenames of approximately 1/3 of all files)

```
cargo run --release --bin tomestone-dump -- discover_paths
```

### tomestone-dump exd
This subcommand prints the contents of a set of tabular EXDF files, containing text and numbers.

```
cargo run --release --bin tomestone-dump -- exd WebURL
```

### tomestone-dump list
This subcommand lists all files, or all files in a particular data pack.

```
cargo run --release --bin tomestone-dump -- list
cargo run --release --bin tomestone-dump -- list exd
cargo run --release --bin tomestone-dump -- list game_script
cargo run --release --bin tomestone-dump -- list music/ex1
```

### tomestone-dump grep
This subcommand searches for a regular expression and lists matching files. The search can optionally be limited to one data pack.

```
cargo run --release --bin tomestone-dump -- grep Carteneau exd
cargo run --release --bin tomestone-dump -- grep TAMTARA game_script
cargo run --release --bin tomestone-dump -- grep -i "Y'shtola"
```

### tomestone-dump hex
This subcommand prints a hex dump of a file to standard output.

```
cargo run --release --bin tomestone-dump -- hex exd/WebURL.exh
cargo run --release --bin tomestone-dump -- hex exd/WebURL_0_en.exd
```

### tomestone-dump raw
This subcommand prints the raw contents of a file to standard output. It should usually be redirected to a file or another command.

```
cargo run --release --bin tomestone-dump -- raw exd/root.exl
cargo run --release --bin tomestone-dump -- raw game_script/system/retainer.luab > retainer.luab
cargo run --release --bin tomestone-dump -- raw bgcommon/hou/indoor/general/0019/asset/fun_b0_m0019.sgb | strings
```
