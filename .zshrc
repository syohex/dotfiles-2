# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh


# Setup terminal, and turn on colors 颜色配置 --- xuehao.zhou
export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=Gxfxcxdxbxegedabagacad
# Enable color in grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='3;33'
# --end


# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
# ZSH_THEME="cloud"
# ZSH_THEME="ys"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# added by robertzhouxh to select vim7.4 instead of vim7.3 --xuehao.zhou
alias vi="/usr/local/bin/vim"
set -o vi

# vim mode in zsh
bindkey -v
bindkey -M viins ‘jj’ vi-cmd-mode
# vim mode in zsh end

# Directory movement
alias up="cd .."
alias bk="cd -"
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias godesk='cd ~/Desktop'

# Directory information
alias ls="ls --color=auto"
alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias dus="du -smh * | sort -nr" #根据文件大小排序
alias lh='ls -d .*' # show hidden files/directories only #只显示隐藏文件
alias ld="ls -lih"
alias map='traceroute' #路由跟踪
alias tgz='tar -czf' #tar -czf [文件名] [被压缩文件] gzip压缩
alias ugz='tar -xzf' #tar -xzf [被压缩文件]
alias pwd='pwd && pwd | pbcopy' #查看当前路径并且复制

alias cleanDS='find . -name ".DS_Store" -print0 | xargs -0 rm -rf' #清除目录下的 DS_Store文件

fucntion mkd() {
    mkdir -p "$@" && cd "$@"
}

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

# Servers
alias vps_81="ssh molmc@143.89.46.81"
alias aliyun="ssh root@112.124.114.164"
alias amz="ssh -i ~/Downloads/xuehao-key-pairs-singapore.pem ubuntu@52.74.148.234"

# Quick Edit
ze(){
   vim ~/.zshrc
}
hosts(){
   sudo vim /etc/hosts
}
vimrc(){
   vim ~/.vimrc #编辑vim配置
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

# 快速查看本机IP地址
function myip() {
    myip="$(ifconfig | grep 'inet.*netmask.*broadcast')"
    lanip="$(echo $myip | awk '{print $2}')"
    publicip="$(echo $myip | awk '{print $6}')"
    echo '你的局域网IP是: '$lanip
    echo '你的外网IP是: '$publicip
}

# Mac Only
alias dns="dscacheutil -flushcache"
alias oo='open .' # open current directory in OS X Finder


# --end

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

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
plugins=(git node brew vi-mode)
plugins=(autojump)
source $ZSH/oh-my-zsh.sh

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/openresty/nginx/sbin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Setup or dismiss (goagent) proxy for curl, wget, etc.
# https_proxy=http://robertzhouxh:Haoge831025@localhost:8787
alias goproxy='export http_proxy=http://127.0.0.1:8087 https_proxy=http://127.0.0.1:8087'
alias noproxy='unset http_proxy https_proxy'

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
