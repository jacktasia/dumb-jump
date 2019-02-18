FROM travisci/ci-connie:packer-1517251212-f485a7f

USER travis
WORKDIR /home/travis
RUN sudo add-apt-repository ppa:gekkio/ag -y
RUN sudo add-apt-repository ppa:git-core/ppa -y
RUN sudo apt-get -qq update || ls
RUN sudo apt-get install -y --no-install-recommends silversearcher-ag git
RUN curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb && sudo dpkg -i ripgrep_0.10.0_amd64.deb && rm ripgrep_0.10.0_amd64.deb
ENV PATH="/home/travis/.evm/bin:$PATH"
ENV PATH="/home/travis/.cask/bin:$PATH"
ENV EVM_EMACS=emacs-24.3-travis
RUN curl -fsSkL https://gist.github.com/rejeep/ebcd57c3af83b049833b/raw > x.sh && bash ./x.sh
RUN evm install $EVM_EMACS --use --skip
RUN wget -q https://dl.google.com/go/go1.11.5.linux-amd64.tar.gz && sudo tar -C /usr/local -xzf go1.11.5.linux-amd64.tar.gz && rm go1.11.5.linux-amd64.tar.gz
ENV PATH=$PATH:/usr/local/go/bin
# RUN git clone https://github.com/jacktasia/dumb-jump.git && cd dumb-jump && cask
RUN rg --version #
RUN sudo usermod -a -G root travis
COPY test-runner-docker-entry.sh test-runner-docker-entry.sh
ENTRYPOINT ["bash", "test-runner-docker-entry.sh"]
