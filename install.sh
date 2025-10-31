#!/bin/bash

# dotfiles in ${HOME}
echo "Creating symlinks under $HOME..."
for f in dot/*; do
    ln -s "$(realpath "$f")" "$HOME/.$(basename "$f")"
done

# dotfiles in ${HOME}/.config
mkdir -p ${HOME}/.config
echo "Creating symlinks under $HOME/.config..."
for d in config/*; do
    ln -Ts `realpath $d` $HOME/.${d}
done

# a couple of binaries and scripts
echo "Creating symlinks to binaries and scripts..."
mkdir -p $HOME/.mylocal
mkdir -p $HOME/.mylocal/bin
for f in bin/*; do
    ln -s `realpath $f` $HOME/.mylocal/bin/`basename $f`
done

# generate ~/.Renviron file with necessary contents
echo "Generating contents of $HOME/.Renviron..."

# extract GitHub access token first (if it's even present)
GITHUB_PAT=$(awk -F= '/GITHUB_PAT/{print $2}' $HOME/.Renviron)

echo "PATH=$PATH" > $HOME/.Renviron
if [[ "$OSTYPE" == "darwin"* ]]; then
    mkdir -p $HOME/.mylocal/R_LIBS
    echo "R_LIBS_USER=$HOME/.mylocal/R_LIBS" >> $HOME/.Renviron
elif [[ ! -f /.dockerenv ]]; then
    echo "R_LIBS_USER=$HOME/projects/.R_LIBS" >> $HOME/.Renviron
fi

# keep around GitHub access token if present
if [[ -n $GITHUB_PAT ]]; then
    echo "Generating GitHub access token..."
    echo GITHUB_PAT=$GITHUB_PAT >> $HOME/.Renviron
else
    echo "Skipping GitHub access token..."
fi

