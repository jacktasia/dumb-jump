# Fail on the first error so a broken install step can't silently skip
# the searcher-gated tests (they are wrapped in `(when (...-installed?) ...)').
set -e

sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends software-properties-common curl
sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends silversearcher-ag ugrep git ruby
sudo curl -LO https://github.com/BurntSushi/ripgrep/releases/download/15.1.0/ripgrep_15.1.0-1_amd64.deb && sudo dpkg -i ripgrep_15.1.0-1_amd64.deb

#ENV PATH="/root/.evm/bin:$PATH"
#ENV PATH="/root/.cask/bin:$PATH"

sudo apt-get install -y --no-install-recommends make libgtk2.0-0 libsm-dev

# Confirm the searchers the test suite depends on are really available.
ag --version
ugrep --version
rg --version
