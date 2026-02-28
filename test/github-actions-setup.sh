sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends software-properties-common curl
sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends silversearcher-ag git python ruby
sudo curl -LO https://github.com/BurntSushi/ripgrep/releases/download/15.1.0/ripgrep_15.1.0-1_amd64.deb && sudo dpkg -i ripgrep_15.1.0-1_amd64.deb

#ENV PATH="/root/.evm/bin:$PATH"
#ENV PATH="/root/.cask/bin:$PATH"

sudo apt-get install -y --no-install-recommends make libgtk2.0-0 libsm-dev
