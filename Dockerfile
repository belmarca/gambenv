FROM debian:trixie-slim

# Install base dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    git \
    autoconf \
    automake \
    libtool \
    pkg-config \
    texinfo \
    wget \
    curl \
    ca-certificates \
    gnupg \
    lsb-release \
    time \
    tree \
    vim \
    gcc-14 \
    g++-14 \
    clang-18 \
# IMPORTANT!
    autoconf2.69 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Create convenient compiler symlinks
RUN ln -sf /usr/bin/gcc-14 /usr/local/bin/gcc-latest && \
    ln -sf /usr/bin/g++-14 /usr/local/bin/g++-latest && \
    ln -sf /usr/bin/clang-18 /usr/local/bin/clang-latest && \
    ln -sf /usr/bin/clang++-18 /usr/local/bin/clang++-latest

# Create gambit user
RUN useradd -m -s /bin/bash gambit

# Switch to gambit user
USER gambit
WORKDIR /home/gambit

# Copy setup and management scripts
COPY --chown=gambit:gambit gambenv.sh /home/gambit/
COPY --chown=gambit:gambit gambit.sh /home/gambit/
COPY --chown=gambit:gambit simple-builder.sh /home/gambit/

# Make scripts executable
RUN chmod +x /home/gambit/gambenv.sh /home/gambit/gambit.sh /home/gambit/simple-builder.sh

# Set environment variables
ENV GAMBIT_ENV_DIR=/home/gambit/.gambit_env
ENV GB_DOTGB=/home/gambit/.gambit_env
ENV PATH=/home/gambit/.gambit_env/bin:/usr/local/bin:$PATH
# Don't ask for confirmation when setting up the environment
ENV GB_FORCE=1

# Source gambit.sh in bashrc for interactive use
RUN echo '. /home/gambit/gambit.sh' >> /home/gambit/.bashrc

# Add convenient aliases
RUN echo 'alias gb-status="./simple-builder.sh status"' >> /home/gambit/.bashrc

CMD ["/bin/bash"]
