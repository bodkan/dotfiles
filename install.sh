#!/bin/bash

for f in dot/*; do
    ln -sv `realpath $f` ~/.`basename $f`
done

[ ! -d ~/.emacs.d ] && ln -sv `realpath emacs.d/` ${HOME}/.emacs.d

mkdir -p ~/.my_local/R_LIBS

echo "PATH=$PATH" > ~/.Renviron
echo "R_LIBS_USER=~/.my_local/R_LIBS" >> ~/.Renviron
echo "RETICULATE_PYTHON=~/.pyenv/versions/retipy/bin/python3" >> ~/.Renviron

