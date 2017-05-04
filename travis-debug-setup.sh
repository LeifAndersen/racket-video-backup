#!/usr/bin/env bash

export RACKET_DIR=~/racket
export RACKET_VERSION=HEAD

sudo apt-get -qq update
sudo apt-get install -y libmlt6
sudo apt-get install -y libav-tools
sudo apt-get install -y ladspa-sdk
sudo apt-get install -y libgdk-pixbuf2.0-*
sudo apt-get install -y frei0r-plugins*
sudo apt-get install libdc1394-*
git clone https://github.com/greghendershott/travis-racket.git ~/travis-racket
cat ~/travis-racket/install-racket.sh | bash # pipe to bash not sh!
export PATH="${RACKET_DIR}/bin:${PATH}" #install-racket.sh can't set for us
export DISPLAY=:99.0
sh -e /etc/init.d/xvfb start
sleep 3 # give xvfb some time to start
raco pkg install --deps search-auto -n video
# raco test -p video
# raco setup --check-pkg-deps --pkgs video
# raco pkg install --deps search-auto cover cover-coveralls
# raco cover -b -f coveralls -d $TRAVIS_BUILD_DIR/coverage .
