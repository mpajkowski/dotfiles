autoload -Uz compinit && compinit
zstyle ':completion:*:*:*' menu yes select
zstyle ':vcs_info:git:*' formats ' [%b]'

setopt prompt_subst
setopt no_nomatch

autoload -Uz vcs_info
precmd() { vcs_info }

PROMPT='%F{red}%~%F{blue}${vcs_info_msg_0_}%F{white} →> '

export EDITOR=nvim

export SAVEHIST=10000
export HISTFILE="$HOME/.history"
setopt hist_ignore_all_dups

setopt autocd
setopt correctall

autoload -z promptinit

# diff-so-fancy
PATH=$PATH:$HOME/diff-so-fancy:$HOME/.cargo

alias vim="nvim"
alias gits="git status"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
