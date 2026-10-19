
# How to install schemesh on Android, using Termux

The preferred Android command-line environment for running schemesh is [Termux](https://termux.dev/)


### Install Chez Scheme on Termux

Chez Scheme is required to compile and run schemesh. To build it from sources, install its prerequisites:
```shell
apt update
apt install build-essential liblz4 ncurses wget zlib
```

then download latest Chez Scheme sources:
Note: it's recommended to download **latest** Chez Scheme from github
because version 1.4.0 lacks some fixes for Android, especially for 32-bit systems.
```shell
git clone --filter=blob:none https://github.com/cisco/ChezScheme
```

Compile them:<br/>
Note: if environment variable `$PREFIX` is not set, run `export PREFIX=/data/data/com.termux/files/usr`
```shell
cd ChezScheme
./configure --prefix="$PREFIX/local" --disable-iconv --disable-x11
make -j `nproc`
```

finally, install it:
```shell
make install
```

Recommended: test that Chez Scheme installation works. Type `scheme` and it should display
```
Chez Scheme Version 10.5.0-pre-release.1
Copyright 1984-2025 Cisco Systems, Inc.

>
```
Type `(exit)` or CTRL+D to quit it.

You can now build schemesh as described below

### Install Schemesh on Termux

Having installed Chez Scheme as described above, schemesh can be installed with:
```shell
apt update
apt install build-essential git ncurses
git clone https://github.com/cosmos72/schemesh
cd schemesh
make -j prefix="$PREFIX/local"

# try schemesh without installing it
./schemesh --library-dir .

# install schemesh
make install prefix="$PREFIX/local"
```

Note: if environment variable `$PREFIX` is not set, run `export PREFIX=/data/data/com.termux/files/usr` and try again
