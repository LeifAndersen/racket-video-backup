#!/usr/bin/env bash
export RACKET_VERSION=HEAD

  sudo add-apt-repository -y ppa:jonathonf/ffmpeg-3
  sudo add-apt-repository -y ppa:jonathonf/tesseract
  sudo apt-get -qq update
  sudo apt-get install libasound2-dev alsa-utils alsa-oss
  sudo modprobe snd-dummy 
  sudo apt-get install -y ffmpeg
  sudo apt-get install -y libav-tools
  sudo apt-get install -y libavcodec-dev
  sudo apt-get install -y libavcodec-extra
  sudo apt-get install -y libavformat-dev
  sudo apt-get install -y libavutil-dev
  sudo apt-get install -y libswscale-dev
  sudo apt-get install -y ladspa-sdk
  sudo apt-get install -y libgdk-pixbuf2.0-*
  sudo apt-get install -y frei0r-plugins*
  sudo apt-get install libdc1394-*
  sudo apt-get install libportaudio2
  # Turn off the irrelevant libdc1394 warning and ALSA warnings
  sudo ln /dev/null /dev/raw1394
  export AUDIODEV=null
  # racket
  git clone https://github.com/greghendershott/travis-racket.git ~/travis-racket
  cat ~/travis-racket/install-racket.sh | bash # pipe to bash not sh!
  export PATH="${RACKET_DIR}/bin:${PATH}" #install-racket.sh can't set for us
  # xvfb, enables gtk
  export DISPLAY=:99.0
  sh -e /etc/init.d/xvfb start
  sleep 3 # give xvfb some time to start
 raco pkg install --deps search-auto -n video

