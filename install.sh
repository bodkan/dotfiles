#!/bin/bash

# create symlinks for all files under `./dot`
for f in dot/*; do
    ln -sv `realpath $f` ~/.`basename $f`
done

# link the entire ~/.emacs.d/ config directory
# [ ! -d ~/.emacs.d ] && ln -sv `realpath emacs.d/` ${HOME}/.emacs.d

mkdir ~/.my_local
mkdir ~/.my_local/bin

# generate ~/.Renviron file with necessary configs
echo "PATH=$PATH" > ~/.Renviron
if [[ "$OSTYPE" == "darwin"* ]]; then
    mkdir ~/.my_local/R_LIBS
    echo "R_LIBS_USER=~/.my_local/R_LIBS" >> ~/.Renviron
else
    echo "R_LIBS_USER=~/projects/.R_LIBS" >> ~/.Renviron
fi
echo "R_BUILD_TAR=tar" >> ~/.Renviron
