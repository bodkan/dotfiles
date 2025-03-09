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

for f in bin/*; do
    ln -sv `realpath $f` $HOME/.my_local/bin/`basename $f`
done

mkdir -p $HOME/.config/rstudio/keybindings
ln -sv `realpath rstudio-prefs.json` $HOME/.config/rstudio/rstudio-prefs.json
ln -sv `realpath rstudio_bindings.json` $HOME/.config/rstudio/keybindings/rstudio_bindings.json

# generate ~/.Renviron file with necessary configs
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

# install vim plugins

mkdir -p ~/.vim/pack/tpope/start
cd ~/.vim/pack/tpope/start
git clone https://tpope.io/vim/surround.git
vim -u NONE -c "helptags surround/doc" -c q

mkdir -p ~/.vim/pack/airblade/start
cd ~/.vim/pack/airblade/start
git clone https://github.com/airblade/vim-gitgutter.git
vim -u NONE -c "helptags vim-gitgutter/doc" -c q

