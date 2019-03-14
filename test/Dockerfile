FROM ubuntu:14.04

RUN apt-get -qq update || ls
RUN apt-get install -y --no-install-recommends software-properties-common curl && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
RUN add-apt-repository ppa:gekkio/ag -y
RUN add-apt-repository ppa:git-core/ppa -y
RUN apt-get -qq update || ls
RUN apt-get install -y --no-install-recommends silversearcher-ag git python ruby && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
RUN curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb && sudo dpkg -i ripgrep_0.10.0_amd64.deb && rm ripgrep_0.10.0_amd64.deb && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
ENV PATH="/root/.evm/bin:$PATH"
ENV PATH="/root/.cask/bin:$PATH"
ENV EVM_EMACS=emacs-24.3-travis

RUN git clone https://github.com/rejeep/evm.git /root/.evm
RUN evm config path /tmp
RUN evm install emacs-24.3-travis --use --skip
RUN curl -fsSkL https://raw.github.com/cask/cask/master/go | python

RUN evm install $EVM_EMACS --use --skip

RUN apt-get install -y --no-install-recommends make libgtk2.0-0 libsm-dev && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true

ADD test/circleci-runner.sh circleci-runner.sh
ENTRYPOINT ["bash", "circleci-runner.sh"]
