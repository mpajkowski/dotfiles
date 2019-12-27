autoload -Uz compinit && compinit

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[3~"   delete-char
bindkey "^[[H"    beginning-of-line
bindkey "^[[F"    end-of-line

zstyle ':completion:*:*:*' menu yes select
zstyle ':vcs_info:git:*' formats ' [%b]'

setopt prompt_subst
setopt no_nomatch

autoload -Uz vcs_info
precmd() { vcs_info }

PROMPT='%F{red}%~%F{blue}${vcs_info_msg_0_}%F{white} > '

export EDITOR=nvim

export SAVEHIST=10000
export HISTFILE="$HOME/.zsh_history"
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory

autoload -z promptinit

# diff-so-fancy
PATH=$PATH:$HOME/diff-so-fancy:$HOME/.cargo/bin

alias vim="nvim"
alias gits="git status"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
