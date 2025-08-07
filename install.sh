#!/bin/bash

for f in dot/*; do
    if [[ -d "$f" ]]; then
        ln -snv "$(realpath "$f")" "$HOME/.$(basename "$f")"
    elif [[ -f "$f" ]]; then
        ln -sv "$(realpath "$f")" "$HOME/.$(basename "$f")"
    fi
done

mkdir $HOME/.my_local
mkdir $HOME/.my_local/bin

# dotfiles in ${HOME}
for f in bin/*; do
    ln -sfv `realpath $f` $HOME/.my_local/bin/`basename $f`
done

# dotfiles in ${HOME}/.config
for d in config/*; do
    ln -sfv `realpath $d` $HOME/.${d}
done

# generate ~/.Renviron file with necessary contents
echo "PATH=$PATH" > $HOME/.Renviron
if [[ "$OSTYPE" == "darwin"* ]]; then
    mkdir -p $HOME/.my_local/R_LIBS
    echo "R_LIBS_USER=$HOME/.my_local/R_LIBS" >> $HOME/.Renviron
elif [[ ! -f /.dockerenv ]]; then
    echo "R_LIBS_USER=$HOME/projects/.R_LIBS" >> $HOME/.Renviron
fi

# keep around GitHub access token if present
if [[ -f $HOME/.Renviron ]]; then
    GITHUB_PAT=$(awk -F= '/GITHUB_PAT/{print $2}' $HOME/.Renviron)
    echo GITHUB_PAT=$GITHUB_PAT >> $HOME/.Renviron
fi

