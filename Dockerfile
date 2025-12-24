FROM gcc:10

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    wget \
    build-essential \
    automake \
    libtool \
    flex \
    bison \
    gawk \
    pkg-config \
    libgmp-dev \
    libdb-dev \
    libncurses5-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN wget https://downloads.sourceforge.net/project/gnucobol/gnucobol/3.2/gnucobol-3.2.tar.gz -O gnucobol.tar.gz && \
    tar -xzf gnucobol.tar.gz && \
    cd gnucobol-3.2 && \
    ./configure CFLAGS="-O2 -Wno-error" && \
    make -j$(nproc) && \
    make install && \
    ldconfig && \
    cd / && \
    rm -rf /tmp/*

WORKDIR /usr/src/app
