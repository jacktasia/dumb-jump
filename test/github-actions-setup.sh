export RG_VER=14.1.0

sudo apt-get -qq update || ls
sudo apt-get install -y --no-install-recommends software-properties-common curl git make
sudo curl -LO https://github.com/BurntSushi/ripgrep/releases/download/${RG_VER}/ripgrep_${RG_VER}-1_amd64.deb && sudo dpkg -i ripgrep_${RG_VER}-1_amd64.deb
