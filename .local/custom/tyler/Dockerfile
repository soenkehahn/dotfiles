FROM custom-ubuntu-base
RUN curl https://sh.rustup.rs -sSf > installer.sh
RUN bash installer.sh -y

ENV version=36082ec
RUN git clone https://github.com/soenkehahn/tyler
WORKDIR tyler
RUN git checkout $version
RUN ~/.cargo/bin/cargo install
WORKDIR /root/.cargo
RUN apack /root/result.tar.gz bin/tyler

