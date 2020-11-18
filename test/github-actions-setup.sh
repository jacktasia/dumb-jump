
cask install
sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends software-properties-common curl && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends silversearcher-ag git python ruby && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
sudo curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb && sudo dpkg -i ripgrep_0.10.0_amd64.deb && rm ripgrep_0.10.0_amd64.deb && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
#ENV PATH="/root/.evm/bin:$PATH"
#ENV PATH="/root/.cask/bin:$PATH"


sudo apt-get install -y --no-install-recommends make libgtk2.0-0 libsm-dev && rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true
