# THIS FILE WAS AUTO-GENERATED
#
#  $ lcitool dockerfile --cross i686 debian-10 libnbd
#
# https://gitlab.com/libvirt/libvirt-ci/-/commit/5fd2cdeb8958df86a81b639fd096f626d0f5dbd0

FROM docker.io/library/debian:10-slim

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y eatmydata && \
    eatmydata apt-get dist-upgrade -y && \
    eatmydata apt-get install --no-install-recommends -y \
            autoconf \
            automake \
            bash-completion \
            ca-certificates \
            ccache \
            coreutils \
            diffutils \
            flake8 \
            git \
            gnutls-bin \
            golang \
            iproute2 \
            jq \
            libtool-bin \
            locales \
            make \
            nbd-client \
            nbd-server \
            nbdkit \
            ocaml \
            ocaml-findlib \
            perl-base \
            pkgconf \
            python3-dev \
            qemu-kvm \
            qemu-utils \
            sed && \
    eatmydata apt-get autoremove -y && \
    eatmydata apt-get autoclean -y && \
    sed -Ei 's,^# (en_US\.UTF-8 .*)$,\1,' /etc/locale.gen && \
    dpkg-reconfigure locales && \
    dpkg-query --showformat '${Package}_${Version}_${Architecture}\n' --show > /packages.txt && \
    mkdir -p /usr/libexec/ccache-wrappers && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/i686-linux-gnu-c++ && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/i686-linux-gnu-cc && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/i686-linux-gnu-g++ && \
    ln -s /usr/bin/ccache /usr/libexec/ccache-wrappers/i686-linux-gnu-gcc

RUN export DEBIAN_FRONTEND=noninteractive && \
    dpkg --add-architecture i386 && \
    eatmydata apt-get update && \
    eatmydata apt-get dist-upgrade -y && \
    eatmydata apt-get install --no-install-recommends -y dpkg-dev && \
    eatmydata apt-get install --no-install-recommends -y \
            g++-i686-linux-gnu \
            gcc-i686-linux-gnu \
            libc6-dev:i386 \
            libev-dev:i386 \
            libfuse3-dev:i386 \
            libglib2.0-dev:i386 \
            libgnutls28-dev:i386 \
            libxml2-dev:i386 && \
    eatmydata apt-get autoremove -y && \
    eatmydata apt-get autoclean -y && \
    mkdir -p /usr/local/share/meson/cross && \
    echo "[binaries]\n\
c = '/usr/bin/i686-linux-gnu-gcc'\n\
ar = '/usr/bin/i686-linux-gnu-gcc-ar'\n\
strip = '/usr/bin/i686-linux-gnu-strip'\n\
pkgconfig = '/usr/bin/i686-linux-gnu-pkg-config'\n\
\n\
[host_machine]\n\
system = 'linux'\n\
cpu_family = 'x86'\n\
cpu = 'i686'\n\
endian = 'little'" > /usr/local/share/meson/cross/i686-linux-gnu

ENV LANG "en_US.UTF-8"
ENV MAKE "/usr/bin/make"
ENV CCACHE_WRAPPERSDIR "/usr/libexec/ccache-wrappers"

ENV ABI "i686-linux-gnu"
ENV CONFIGURE_OPTS "--host=i686-linux-gnu"
