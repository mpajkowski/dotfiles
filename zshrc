if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

setopt NO_NOMATCH
prompt cloud

export EDITOR=nvim


# diff-so-fancy
PATH=$PATH:$HOME/diff-so-fancy:$HOME/.cargo

alias vim="nvim"
alias gits="git status"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
