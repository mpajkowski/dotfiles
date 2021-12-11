fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/diff-so-fancy"
fish_add_path "$HOME/.fzf/bin"
fish_add_path "/snap/bin"
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

export MOZ_ENABLE_WAYLAND=1

fish_vi_key_bindings
