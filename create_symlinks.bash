#!/usr/bin/env bash

declare -A config_files=(
  ["INIT_VIM"]="$HOME/.config/nvim/init.vim"
  ["I3_CONFIG"]="$HOME/.config/i3/config"
  ["TMUX_CONF"]="$HOME/.tmux.conf"
  ["COC_SETTINGS"]="$HOME/.config/nvim/coc-settings.json"
)

for file in "${!config_files[@]}"; do
  path=${config_files[$file]}
  if [[ -e $path ]] ; then
    echo "$path exists, skipping..."
    continue
  fi

  local_path=$(echo $path | awk -F '/' '{ print $NF }')
  ln -s $(pwd)/$local_path $path
done
