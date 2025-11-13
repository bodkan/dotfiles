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

# generate ~/.Renviron file with necessary contents
echo "Generating contents of $HOME/.Renviron..."

# extract GitHub access token first (if it's even present)
GITHUB_PAT=$(awk -F= '/GITHUB_PAT/{print $2}' $HOME/.Renviron)

echo "PATH=$PATH" > $HOME/.Renviron
if [[ ! -f /.dockerenv ]] && [[ "$OSTYPE" != "darwin"* ]]; then
    echo "R_LIBS_USER=$HOME/projects/.R_LIBS" >> $HOME/.Renviron
fi
