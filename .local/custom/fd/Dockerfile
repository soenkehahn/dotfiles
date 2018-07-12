FROM custom-ubuntu-base
RUN curl https://sh.rustup.rs -sSf > installer.sh
RUN bash installer.sh -y

RUN ~/.cargo/bin/cargo install fd-find
WORKDIR /root/.cargo
RUN apack /root/result.tar.gz bin/fd

