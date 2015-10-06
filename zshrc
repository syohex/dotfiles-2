# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
# ZSH_THEME="eastwood"
ZSH_THEME="agnoster"


# Set my terminal if I'm not in tmux.
[[ "$TMUX" == "" ]] && TERM=xterm-256color

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git colored-man colorize github jira vagrant virtualenv pip python brew osx zsh-syntax-highlighting autojump)



# ======================= User configuration =======================
# export MANPATH="/usr/local/man:$MANPATH"
export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:/usr/local/lib/erlang/man:$MANPATH

# Add locations to the $PATH that actually exist.
PATH="/usr/local/bin:$PATH"
[ -d "/usr/texbin" ]               && PATH="/usr/texbin:$PATH"
[ -d "/usr/local/share/npm/bin" ]  && PATH="/usr/local/share/npm/bin:$PATH"
[ -d "$HOME/PebbleSDK-2.0.0/bin" ] && PATH="$HOME/PebbleSDK-2.0.0/bin:$PATH"

# Add my scripts directory to my path if it's there.
if [ -d "$HOME/bin" ]; then
  PATH="$HOME/bin:$PATH"
fi

# use Unix Style cmd
export PATH=/usr/local/opt/coreutils/libexec/gnubin:/opt/local/libexec/gnubin:$PATH


source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  # export EDITOR='vim'
else
  # export EDITOR='emacs'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
alias ls="ls --color=auto"
alias ll="ls -lAFh --color"
alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias dus="du -smh * | sort -nr" #根据文件大小排序
alias lh='ls -d .*' # show hidden files/directories only #只显示隐藏文件
alias ld="ls -lih"
alias map='traceroute' #路由跟踪
alias tgz='tar -czf' #tar -czf [文件名] [被压缩文件] gzip压缩
alias ugz='tar -xzf' #tar -xzf [被压缩文件]
alias pwd='pwd && pwd | pbcopy' #查看当前路径并且复制
alias cleanDS='find . -name ".DS_Store" -print0 | xargs -0 rm -rf' #清除目录下的 DS_Store文件


# 列出系统最大的文件 快捷 maxfile 即可
maxfile() {
   lsof / | awk '{ if($7 > 1048576) print $7/1048576 "MB "$9 }' | sort -n -u | tail
}

extract () {
    if [ -f $1 ] ; then
      case $1 in
        *.tar.bz2)   tar xjf $1     ;;
        *.tar.gz)    tar xzf $1     ;;
        *.bz2)       bunzip2 $1     ;;
        *.rar)       unrar e $1     ;;
        *.gz)        gunzip $1      ;;
        *.tar)       tar xf $1      ;;
        *.tbz2)      tar xjf $1     ;;
        *.tgz)       tar xzf $1     ;;
        *.zip)       unzip $1       ;;
        *.Z)         uncompress $1  ;;
        *.7z)        7z x $1        ;;
        *)     echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

# Quick Edit
zshrc(){
   vim ~/.zshrc
}
hosts(){
   sudo vim /etc/hosts
}
vimrc(){
   vim ~/.vimrc #编辑vim配置
}

emacs.d(){
   vim ~/.emacs.d #编辑 emacs
}


# Git
alias gam="git commit -a -m"
alias gc= "git checkout"
alias gs="git status"
alias gp='git push'
alias gl="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit "
alias gb='git branch'
alias gd='git diff' #显示缓存变化
alias ghard='git reset --hard'
alias ssh='ssh -A'

## Emacs:
## alias emacs="`which emacs` -nw"   # Failsafe
alias e="emacsclient -t"          # Preferred
alias se='SUDO_EDITOR="emacsclient -t" sudo -e'
alias ed="`which emacs` --daemon" # Start the daemon

# 让emacs -t 成为默认编辑器让svn git 方便提交
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=""
export VISUAL="emacsclient -c -a emacs"

## I don't know why this isn't configured by default.
export LD_LIBRARY_PATH=/usr/local/lib


# 快速查看本机IP地址
function myip() {
    myip="$(ifconfig | grep 'inet.*netmask.*broadcast')"
    lanip="$(echo $myip | awk '{print $2}')"
    publicip="$(echo $myip | awk '{print $6}')"
    echo '你的局域网IP是: '$lanip
    echo '你的外网IP是: '$publicip
}


alias grep="grep --color=auto"
alias vi='vim'
alias -s html=vim # 在命令行直接输入后缀为 html 的文件名，会在 TextMate 中打开
alias -s rb=vi
alias -s py=vi
alias -s js=vi
alias -s txt=vi
alias -s org=emacs
alias -s c=emacs
alias -s erl=emacs
alias -s org=emacs
alias -s md=emacs

# Mac Only
alias dns="dscacheutil -flushcache"
alias oo='open .' # open current directory in OS X Finder

# FileSearch
function f() { find . -iname "*$1*" ${@:2} }
function r() { grep "$1" ${@:2} -R . }

#mkdir and cd
function mkcd() { mkdir -p "$@" && cd "$_"; }


# vi mode
bindkey -v
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^K" kill-line
bindkey "^R" history-incremental-search-backward
bindkey "^P" history-search-backward
bindkey "^Y" accept-and-hold
bindkey "^N" insert-last-word

# allow ctrl-h, ctrl-w, ctrl-? for char and word deletion (standard behaviour)
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey -s "^T" "^[Isudo ^[A" # "t" for "toughguy"

# makes color constants available
autoload -U colors
colors

# history settings
setopt hist_ignore_all_dups inc_append_history
HISTFILE=~/.zhistory
HISTSIZE=4096
SAVEHIST=4096

# autojump
[[ -s ~/.autojump/etc/profile.d/autojump.sh ]] && . ~/.autojump/etc/profile.d/autojump.sh

# Local config
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
