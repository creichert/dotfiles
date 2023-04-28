# This is almost an exact copy of my workstation setup.
#
# The idea behind recreating my local setup in this Docker image is to
# run CI against my "real" machine and codify a lot of the small hacks
# I use on my workstation.
#
# To see what's installed on Debian / Ubuntu through APT, you can
# $ awk -vRS= '/Status: install/ {print $2}' /var/lib/dpkg/status | grep systemd

FROM assertible/debian-min:unstable

ENV DEBIAN_FRONTEND noninteractive
ENV SHELL /bin/bash

ARG debian_mirror_url=http://httpredir.debian.org/debian
RUN printf "deb %s sid main contrib\n" ${debian_mirror_url} > /etc/apt/sources.list
RUN printf "deb-src %s sid main contrib\n" ${debian_mirror_url} >> /etc/apt/sources.list
RUN cat /etc/apt/sources.list
RUN apt-get update

# Configure utf-8 system locale
RUN apt-get install -y --no-install-recommends \
            locales

RUN locale-gen en_US.UTF-8 en_us && \
               dpkg-reconfigure locales && \
               locale-gen C.UTF-8 && \
               /usr/sbin/update-locale LANG=C.UTF-8

RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8


RUN apt-get install -y --no-install-recommends \
        systemd \
        systemd-container \
        systemd-sysv

# I use curl & libcurl-dev extensively
RUN apt-get install -y --no-install-recommends \
            openssl                            \
            libssl3                            \
            ca-certificates                    \
            curl

# Build dependencies
RUN apt-get install -y --no-install-recommends \
            libgmp10                           \
            libgmp-dev                         \
            make                               \
            xz-utils                           \
            libpq5                             \
            libpq-dev                          \
            libpcre3                           \
            libpcre3-dev                       \
            zlib1g-dev                         \
            gcc

# Javascript dependencies
RUN apt-get install -y --no-install-recommends \
            fontconfig                         \
            pkg-config                         \
            bzip2

# CI utilities
#
# Wishlist:
#   - vim
#   - less
RUN apt-get install -y --no-install-recommends \
            awscli                             \
            zip                                \
            jq                                 \
            git                                \
            shellcheck                         \
            openssh-client                     \
            openssh-server                     \
            docker.io

# x11
RUN apt-get install -y --no-install-recommends \
        xvfb                               \
        xinit                              \
        x11-xserver-utils                  \
        xserver-xorg                       \
        xscreensaver                       \
        unclutter                          \
        libx11-dev                         \
        libxss-dev                         \
        libxrandr-dev                      \
        libxft-dev


# mpris / dunst / dbus utilities
RUN apt-get install -y --no-install-recommends \
        dbus-x11 \
        dbus-user-session


RUN apt-get install -y --no-install-recommends \
        stow \
        emacs


RUN apt-get install -y --no-install-recommends \
        msmtp \
        pass


# Notifications
RUN apt-get install -y --no-install-recommends \
        libnotify-bin \
        dunst

# Audio
RUN apt-get install -y --no-install-recommends \
        pulseaudio \
        pulseaudio-utils \
        alsa-utils


# dev utils
RUN apt-get install -y --no-install-recommends \
        xmlstarlet \
        libxml2

RUN apt-get -y autoremove
RUN apt-get -y autoclean

# emacs & emacs dev libs for hacking
RUN apt-get -y build-dep emacs

# Install nvm
#
# - https://gist.github.com/remarkablemark/aacf14c29b3f01d6900d13137b21db3a#file-dockerfile
ENV NVM_DIR /usr/local/nvm
ARG node_version=10.19.0
ENV NODE_VERSION ${node_version}
ENV NVM_INSTALL_URL https://raw.githubusercontent.com/creationix/nvm/v0.31.2/install.sh
RUN curl --silent -o- $NVM_INSTALL_URL | bash

# Install node and npm

RUN . $NVM_DIR/nvm.sh                  \
    && nvm install $NODE_VERSION       \
    && nvm alias default $NODE_VERSION \
    && nvm use default

# add node and npm to path so the commands are available
ENV NODE_PATH $NVM_DIR/v$NODE_VERSION/lib/node_modules
ENV PATH $NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH

# Install stack
ENV STACK_INSTALL_URL https://github.com/commercialhaskell/stack/releases/download/v2.9.3/stack-2.9.3-linux-x86_64.tar.gz
RUN curl -L $STACK_INSTALL_URL -o /tmp/stack.tar.gz
RUN tar -xvzf /tmp/stack.tar.gz
RUN cp stack-2.9.3-linux-x86_64/stack /usr/local/bin/stack
RUN rm -rf stack-2.9.3-linux-x86_64 /tmp/stack.tar.gz


# Print software versions
RUN uname -snrvm
RUN stack --version
RUN node -v
RUN npm -v

# Use container cmd suitable for CI environment
CMD bash

# Local Variables:
# dockerfile-image-name: "creichert/debian"
# dockerfile-build-args: ("debian_mirror_url=http://debian.cse.msu.edu/debian" "node_version=9.7.1")
# End:
