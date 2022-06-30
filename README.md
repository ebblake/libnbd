## NBD client library in userspace

NBD — Network Block Device — is a protocol for accessing Block Devices
(hard disks and disk-like things) over a Network.  This is the NBD
client library in userspace, a simple library for writing NBD clients.

The key features are:

* Synchronous API for ease of use.
* Asynchronous API for writing non-blocking, multithreaded clients.
  You can mix both APIs freely.
* High performance.
* Minimal dependencies for the basic library.
* Well-documented, stable API.
* Bindings in several programming languages.
* Shell (nbdsh) for command line and scripting.
* Copying tool (nbdcopy) for high performance copying and streaming.
* Hexdump tool (nbddump) to print NBD content.
* Query tool (nbdinfo) to query NBD servers.
* FUSE support (nbdfuse) to mount NBD in the local filesystem.

For documentation, see the [docs](docs/) and [examples](examples/)
subdirectories.


## License

The software is copyright © Red Hat Inc. and licensed under the GNU
Lesser General Public License version 2 or above (LGPLv2+).  See the
file [COPYING.LIB](COPYING.LIB) for details.  The examples are under a
very liberal license.


## Building from source

To build from git:

```
autoreconf -i
./configure
make
make check
```

To build from tarball:

```
./configure
make
make check
```

To run the tests under valgrind:

```
make check-valgrind
```

To run benchmarks:

```
make bench
```

Some tests require root permissions (and are therefore risky).  If you
want to run these tests, do:

```
sudo make check-root
```

Requirements:

* Linux, FreeBSD or OpenBSD.
  Other OSes may also work but we have only tested these three.
* GCC or Clang
* GNU make
* bash

Required for building from git, optional for building from tarballs:

* OCaml (ocamlc, the bytecode compiler) is required to generate some
  source files, which is needed to build from git.  However ocamlc is
  _not_ needed if building from the official tarballs from
  http://download.libguestfs.org/libnbd/ because those contain the
  generated files.

Recommended - if not present, some features will be disabled:

* GnuTLS is recommended for TLS support.
  If not available then you will not be able to access encrypted
  servers and some APIs related to TLS will always return an error.
* libxml2 is recommended for NBD URI support.
  If not available then a few APIs related to URIs will always return
  an error, and the nbdinfo tool is not built.

Optional:

* Perl Pod::Man and Pod::Simple to generate the documentation.
* OCaml and ocamlfind are both needed to generate the OCaml bindings.
* Python >= 3.3 to build the Python 3 bindings and NBD shell (nbdsh).
* FUSE 3 to build the nbdfuse program.
* go and cgo, for compiling the golang bindings and tests.
* bash-completion >= 1.99 for tab completion.

Optional, only needed to run the test suite:

* nbdkit >= 1.12, the nbdkit basic plugins and the nbdkit basic
  filters are recommended as they are needed to run the test suite.
* nbdkit, nbd-server, qemu-nbd and qemu-storage-daemon if you want to
  do interoperability testing against these servers.
* A C++ compiler is needed if you want to test that the library works
  from C++.
* coreutils or standard Unix tools such as cmp, dd, stat, truncate.
* libdl (dlopen, dlclose etc) to test this functionality.
* qemu, qemu-io, qemu-img for interoperability testing.
* certtool and psktool (part of GnuTLS) for testing TLS support.
* valgrind if you want to run the tests under valgrind.
* nbd-client and Linux nbd.ko for testing nbdcopy to/from devices.
* flake8 to keep Python code formatted uniformly.
* ss (from iproute) for TCP-based tests.

Optional, only needed to run some examples:

* glib2 for examples that interoperate with the glib main loop.
* libev for examples that interoperate with the libev library.


### Download tarballs

Tarballs are available from:
http://libguestfs.org/download/libnbd


## Developers

Install the valgrind program and development headers.

Use:

```
./configure --enable-gcc-warnings --enable-python-code-style
```

When testing use:

```
make check
make check-valgrind
```

Use the following one-time setup for nicer diffs of OCaml files:

```
git config diff.ml.xfuncname '^(type|and|val|let) .*='
git config diff.ml-api.xfuncname '^(let .*=|  "[^"]*", \{$)'
git config diff.mli.xfuncname '^(type|and|val|module) '
```

For development ideas, see the [TODO](TODO) file.

The upstream git repository is:
https://gitlab.com/nbdkit/libnbd

Patches are accepted either by email to the upstream mailing list:
https://www.redhat.com/mailman/listinfo/libguestfs
or by Merge Request on gitlab.com

If you want to fuzz the library see [fuzzing/README](fuzzing/README).


## Other links

* http://libguestfs.org/
* https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md
* https://gitlab.com/nbdkit/nbdkit
