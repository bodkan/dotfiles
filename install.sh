#!/bin/bash

for f in dot/*; do
    ln -sv `realpath $f` ~/.`basename $f`
done

[ ! -d ~/.emacs.d ] && ln -sv `realpath emacs.d/` ${HOME}/.emacs.d

echo "PATH=$PATH" > ~/.Renviron
echo "R_LIBS_USER=~/.my_local/R_LIBS" >> ~/.Renviron
