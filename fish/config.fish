set -U fish_user_paths $fish_user_paths "$HOME/.local/bin"
set -U fish_user_paths $fish_user_paths "$HOME/.cargo/bin"
set -U fish_user_paths $fish_user_paths "$HOME/diff-so-fancy"
set -U fish_user_paths $fish_user_paths "$HOME/.fzf/bin"
set -U fish_user_paths $fish_user_paths "/snap/bin"
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
