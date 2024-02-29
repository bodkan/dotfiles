#!/bin/bash

# create symlinks for all files under `./dot`
for f in dot/*; do
    ln -sv `realpath $f` ~/.`basename $f`
done

mkdir ~/.my_local
mkdir ~/.my_local/bin

for f in bin/*; do
    ln -sv `realpath $f` ~/.my_local/bin/`basename $f`
done


mkdir -p ~/.config/rstudio/keybindings
ln -sv `realpath rstudio-prefs.json` ~/.config/rstudio/rstudio-prefs.json
ln -sv `realpath rstudio_bindings.json` ~/.config/rstudio/keybindings/rstudio_bindings.json

# save GitHub access token if present
if [[ -f ~/.Renviron ]]; then
    GITHUB_PAT=$(awk -F= '/GITHUB_PAT/{print $2}' ~/.Renviron)
    echo GITHUB_PAT=$GITHUB_PAT >> ~/.Renviron
fi

# generate ~/.Renviron file with necessary configs
echo "PATH=$PATH" > ~/.Renviron
if [[ "$OSTYPE" == "darwin"* ]]; then
    mkdir -p ~/.my_local/R_LIBS
    echo "R_LIBS_USER=~/.my_local/R_LIBS" >> ~/.Renviron
if [ -z "$IN_DOCKER" ]; then
    echo "R_LIBS_USER=~/projects/.R_LIBS" >> ~/.Renviron
fi
