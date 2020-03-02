autoload -Uz compinit && compinit

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[3~"   delete-char
bindkey "^[[1~"   beginning-of-line
bindkey "^[[4~"   end-of-line

#zstyle ':completion:*:*:*' menu yes select
zstyle ':vcs_info:git:*' formats ' [%b]'

setopt prompt_subst
setopt no_nomatch

autoload -Uz vcs_info
precmd() { vcs_info }

PROMPT='%F{red}%~%F{blue}${vcs_info_msg_0_}%F{white} > '

export EDITOR=vim

##
## History
###
HISTFILE=~/.zsh_history         # where to store zsh config
HISTSIZE=1024                   # big history
SAVEHIST=1024                   # big history
setopt append_history           # append
setopt hist_ignore_all_dups     # no duplicate
unsetopt hist_ignore_space      # ignore space prefixed commands
setopt hist_reduce_blanks       # trim blanks
setopt hist_verify              # show before executing history commands
setopt inc_append_history       # add commands as they are typed, don't wait until shell exit
setopt share_history            # share hist between sessions
setopt bang_hist                # !keyword

setopt auto_cd
setopt auto_pushd               # make cd push old dir in dir stack
setopt pushd_ignore_dups        # no duplicates in dir stack
setopt pushd_silent             # no dir stack after pushd or popd
setopt pushd_to_home            # `pushd` = `pushd $HOME`

autoload -z promptinit

# diff-so-fancy
PATH=$PATH:$HOME/diff-so-fancy:$HOME/.cargo/bin:$HOME/.local/bin

alias gits="git status"
alias gp5='wine "/home/marcin/.wine/drive_c/Program Files (x86)/Guitar Pro 5/GP5.exe"'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
