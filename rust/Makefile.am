# nbd client library in userspace
# Copyright Tage Johansson
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/subdir-rules.mk

generator_built = \
	libnbd-sys/src/generated.rs \
	src/bindings.rs \
	$(NULL)

source_files = \
	$(generator_built) \
	Cargo.toml \
	src/lib.rs \
	src/error.rs \
	src/handle.rs \
	src/types.rs \
	src/utils.rs \
	libnbd-sys/Cargo.toml \
	libnbd-sys/build.rs \
	libnbd-sys/src/lib.rs \
	cargo_test/Cargo.toml \
	cargo_test/src/lib.rs \
	cargo_test/README.md \
	run-tests.sh.in \
	$(NULL)

EXTRA_DIST = \
	$(source_files) \
	$(NULL)

if HAVE_RUST

all-local: libnbd-sys/libnbd_version target/debug/liblibnbd.rlib \
		target/doc/libnbd/index.html

libnbd-sys/libnbd_version: Makefile
	rm -f libnbd-sys/libnbd_version.t
	$(abs_top_builddir)/run echo $(VERSION) > libnbd-sys/libnbd_version.t
	mv libnbd-sys/libnbd_version.t libnbd-sys/libnbd_version

target/debug/liblibnbd.rlib: $(source_files)
	$(abs_top_builddir)/run $(CARGO) build

target/doc/libnbd/index.html: $(source_files)
	$(abs_top_builddir)/run $(CARGO) doc

TESTS_ENVIRONMENT = \
	LIBNBD_DEBUG=1 \
	$(MALLOC_CHECKS) \
	abs_top_srcdir=$(abs_top_srcdir) \
	$(NULL)
LOG_COMPILER = $(top_builddir)/run
TESTS = run-tests.sh

clean-local:
	$(CARGO) clean
	$(CARGO) clean --manifest-path cargo_test/Cargo.toml

endif

CLEANFILES += libnbd-sys/libnbd_version
