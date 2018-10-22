#!/usr/bin/env bash

INIT_VIM=$HOME/.config/nvim/init.vim
I3_CONFIG=$HOME/.config/i3/config
TMUX_CONF=$HOME/.tmux.conf

# TODO: check if symlinks exist
if [[ -e $INIT_VIM ]] ; then
  mv $INIT_VIM ${INIT_VIM}.orig
  echo "Removed $INIT_VIM"
fi

if [[ -e $I3_CONFIG ]] ; then
  mv $I3_CONFIG ${I3_CONFIG}.orig
  echo "Removed $I3_CONFIG"
fi

if [[ -e $TMUX_CONF ]] ; then
  mv $TMUX_CONF ${TMUX_CONF}.orig
  echo "Removed $TMUX_CONF"
fi

ln -s $(pwd)/init.vim $INIT_VIM
ln -s $(pwd)/i3_config $I3_CONFIG
ln -s $(pwd)/tmux.conf $TMUX_CONF
