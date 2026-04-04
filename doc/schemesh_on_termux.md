
# How to install schemesh on Android, using Termux

The preferred Android command-line environment for running schemesh is [Termux](https://termux.dev/)


### Install Chez Scheme on Termux

Chez Scheme is required to compile and run schemesh. To build it from sources, install its prerequisites:
```shell
apt update
apt install build-essential liblz4 ncurses wget zlib
```

then download Chez Scheme sources:
```shell
wget https://github.com/cisco/ChezScheme/releases/download/v10.3.0/csv10.3.0.tar.gz
tar xzf csv10.3.0.tar.gz
```

and compile them:<br/>
Note: if environment variable `$PREFIX` is not set, run `export PREFIX=/data/data/com.termux/files/usr`
```shell
cd csv10.3.0
./configure --prefix="$PREFIX/local" --disable-iconv --disable-x11 LZ4=-llz4 ZLIB=-lz
make -j `nproc`
```


finally, install it:
```shell
make install
```

Recommended: test that Chez Scheme installation works. Type `scheme` and it should display
```
Chez Scheme Version 10.3.0
Copyright 1984-2025 Cisco Systems, Inc.

>
```
Type `(exit)` to quit it.

You can now build schemesh as described below

### Install Schemesh on Termux

Having installed Chez Scheme as described above, schemesh can be installed with:
```shell
apt update
apt install build-essential git liblz4 ncurses zlib
git clone https://github.com/cosmos72/schemesh
cd schemesh
git checkout -f v1.0.0
make -j prefix="$PREFIX/local"

# try schemesh without installing it
./schemesh --library-dir .

# install schemesh
make install prefix="$PREFIX/local"
```

Note: if environment variable `$PREFIX` is not set, run `export PREFIX=/data/data/com.termux/files/usr` and try again
