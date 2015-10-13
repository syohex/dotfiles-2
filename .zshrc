# Path to your oh-my-zsh installation.
export ZSH=/Users/robertzhouxh/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="agnoster"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

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

# User configuration

export PATH="/usr/local/bin:/usr/sbin:/usr/bin:/opt/X11/bin:/Library/TeX/texbin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


# ======================= User configuration =======================
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

# go
# export PATH=$PATH:/usr/local/opt/go/libexec/bin
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/goEnv
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

# use Unix Style cmd
export PATH=/usr/local/opt/coreutils/libexec/gnubin:/opt/local/libexec/gnubin:$PATH

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='emacs'
fi

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"
alias d="cd ~/Documents/Dropbox"
alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias p="cd ~/githubs"
alias h="history"

if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color"
else # OS X `ls`
    colorflag="-G"
fi

alias l="ls -lF ${colorflag}"    # List all files colorized in long format
alias la="ls -laF ${colorflag}"    # List all files colorized in long format, including dot files
alias lsd="ls -lF ${colorflag} | grep --color=never '^d'"   # List only directories
alias ls="command ls ${colorflag}"    # Always use color output for `ls`
alias sudo='sudo '    # Enable aliases to be sudo’ed
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'

alias week='date +%V'   # Get week number
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'    # Stopwatch
alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias dus="du -smh * | sort -nr" #根据文件大小排序
alias lh='ls -d .*' # show hidden files/directories only #只显示隐藏文件
alias ld="ls -lih"
alias map='traceroute' #路由跟踪
alias tgz='tar -czf' #tar -czf [文件名] [被压缩文件] gzip压缩
alias ugz='tar -xzf' #tar -xzf [被压缩文件]
alias pwd='pwd && pwd | pbcopy' #查看当前路径并且复制
alias cleanDS='find . -name ".DS_Store" -print0 | xargs -0 rm -rf' #清除目录下的 DS_Store文件

alias route="netstat -r"
alias listport="netstat -a | egrep 'Proto|LISTEN'"

# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
alias map="xargs -n1"

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
#     alias e="emacsclient -t"          # Preferred
#     alias se='SUDO_EDITOR="emacsclient -t" sudo -e'
#     alias ed="`which emacs` --daemon" # Start the daemon

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

ulimit -n 1048576

# virtual hosts
alias test="ssh -i ~/.ssh/test_rsa zxh@192.168.0.45"

# Local config
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
