#!/usr/bin/env bash

install_flex() {
    sudo apt-get update -qq
    sudo apt-get install -y flex bison build-essential csh \
         libxaw7-dev lib32z1 lib32ncurses5
}

"$@"
