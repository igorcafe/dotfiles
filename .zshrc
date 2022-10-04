# Path to your oh-my-zsh installation.
export ZSH="/home/igor/.oh-my-zsh"

# themes i liked
# ZSH_THEME="arrow"
ZSH_THEME="half-life"
# ZSH_THEME="sorin"
# tail -n +2 ~/.themes > .temp
# ZSH_THEME="$(head -n1 ~/.themes)"
# mv .temp .themes
# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line if pasting URLs and other text is messed up.
DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# plugins
plugins=(
	git
	colored-man-pages
  dirhistory
	sudo
	zsh-autosuggestions
	zsh-syntax-highlighting
)

typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[builtin]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[command]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[alias]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]="fg=black,bold"
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]="fg=black,bold"

source $ZSH/oh-my-zsh.sh

# Preferred editor
export EDITOR='nvim'
export VISUAL='nvim'
alias vim="nvim"
alias v="nvim"
alias vimrc="nvim ~/.config/nvim/init.vim"
alias viter="vim -c ':ter'; exit"

alias ipy=ipython3

# useful :)
function crun () {
  gcc "$@" && ./a.out
}

function cpprun () {
  g++ -Wall -Werror "$@" && ./a.out
}

function bak () {
  mv "$@" "$@.bak"
}

function unbak () {
  mv "$@" "${@%.bak}"
}

function diffs () {
  diff -u $1 $2 | delta
}

function difftext() {
        t1="$1"
        t2="$2"
        r=$RANDOM

        p1="/tmp/t1-$r"
        p2="/tmp/t2-$r"

        echo "$t1" > "$p1"
        echo "$t2" > "$p2"

        diff -u "$p1" "$p2" | delta

        rm $p1 $p2
}

function del () {
  mv $@ ~/.local/share/Trash
}

alias nod="node ~/Git/pessoal/nod"

function quasar () {
  if [[ "$@" == "build" && "$(pwd)" == "/home/igor/Git/essia/essia-frontend-core" ]]; then
    command quasar build && \
    rm -r ../essia-desktop/src/client 2> /dev/null && \
    mkdir -p ../essia-desktop/src/client && \
    cp -r dist/spa/* ../essia-desktop/src/client && \
    echo "-> copiado para essia-desktop"
  else
    command quasar $@
  fi
}

# custom git alias and stuff
alias gal="cat ~/.oh-my-zsh/plugins/git/README.md | grep -A10000 '## Aliases' | grep -B10000 '### Main branch preference' | grep '^| g' | grep -vw 'svn' | fzf"
alias gdc="git diff --cached"
alias glg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gla="git pull --all"
alias gcm="git commit -m"
alias gacm="git add -A && git commit -m"
alias grf="git reflog"
alias grz="git reset"
alias grzs="git reset --soft"
alias grzh!="git reset --hard"
alias gclean!="git clean -f"
alias gstl="git stash list --oneline"
alias gpro="git pull --rebase origin"
alias gprod="git pull --rebase origin develop"
alias grl="git log --pretty=\"%C(reset)%<(7)%C(green)%h%C(reset)   %<(90,trunc)%s  %<(11,trunc)%an   %<(10,trunc)%C(yellow)%ar\" --color"
alias gstk="git stash --keep-index"
alias gstuk="git stash --include-untracked --keep-index"
# alias gstau="git stash push -u"
# alias gstaum="git stash push -um"
alias gwip="git commit --no-verify --no-gpg-sign -m '--wip-- [skip ci]'"

# function grl() {
	# base="--pretty=\"%C(reset)%<(7)%C(green)%h%C(reset)   %<(90,trunc)%s  %<(11,trunc)%an   %<(10,trunc)%C(yellow)%ar\" --color | sed -r 's/([0-9]{2}:[0-9]{2})\.\./\1/g' | less"
	# cmd="git log $base"
	# eval "$cmd"
# }

# checkout to recent ref

function grco() {
  res="$(grf | grep checkout: | egrep -o "to .*" | cut -d' ' -f2 | awk '!x[$0]++' | fzf)"
  [ $? -eq 0 ] && gco "$res"
}

function gac() {
  git add $@ && git commit -m "$@"
}

function grank() {
  WIPE="\e[1A\e[K"
  O="\033[0;33m"
  C="\033[0;36m"
  NC="\033[0m"

  if [[ $# -ge 1 ]]
  then
    for repo in "$@"
    do
      cd $repo
      grank
      cd ..
    done
    return
  fi

  current_line=0
  echo
  echo "$O${PWD##*/} $NC($C$current_line/?)$NC"

  rm .grank-final 2> /dev/null
  rm .grank-temp 2> /dev/null

  git ls-files | xargs -n1 file | grep 'text$' | cut -d':' -f1 > .grank-temp
  lines_count=$(wc -l .grank-temp | cut -d' ' -f1)

  cat .grank-temp | while read -r line
  do
    current_line=$[$current_line + 1]
    echo "$WIPE$O${PWD##*/} $NC($C$current_line/$lines_count)$NC"
    git blame -- "$line" | \
      sd '^.* \((.*) 20\d{2}-\d{2}-\d{2}.*' '$1' | \
      sd '\s*$' '' | \
      egrep -vw 'Not Committed Yet' | \
      sd '^Raquel$' 'Raquel Jacques' | \
      sd 'Pedro Lucas Flor Martins da Silva' 'Pedro Silva' | \
      sd 'unknown' 'Pedro Silva' | \
      sd 'Matheus Alexander M. G. Simão' 'Matheus Alexander' | \
      sd 'Matheus Simão' 'Matheus Alexander' | \
      sd 'marcelo@essia.com' 'Marcelo Tavares' | \
      sd 'gabrielluiz1010' 'Gabriel Oliveira' | \
      sd 'gaoliveiraessia' 'Gabriel Oliveira' | \
      sd 'Gabriel Martins' 'Gabriel Oliveira' | \
      sd '^Gabriel$' 'Gabriel Oliveira' | \
      sd 'robhersonwector' 'Robherson' | \
      sd 'NoteK' 'Kelson' | \
      sd 'psilvaessia' 'Pedro Silva' | \
      sd 'Luiz182' 'Luiz Cesar' | \
      sd 'boliveira' 'Bianca Oliveira' | \
      sd 'Diego Costa Marcelino' 'Diego Marcelino' | \
      sd 'guilhermeoliveira_29' 'Guilherme Oliveira' \
      >> .grank-final
  done

  cat .grank-final | sort | uniq -c | sort -nr

  rm .grank-final .grank-temp 2> /dev/null
  echo
}

# ls but better
EXA_OPTS="-l -h --icons --git -F --color=always"
alias l="exa $EXA_OPTS"
alias la="exa $EXA_OPTS -a"
alias lag="exa $EXA_OPTS -ag"
alias ls="lsd"
alias l="lsd"
alias la="lsd -a"
alias ll="lsd -l"
alias lal="lsd -al"

# grep
alias grep='grep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,node_modules}'

# .zshrc
# alias zshconf="nvim ~/.zshrc" # deprecated
alias zshrc="lvim ~/.zshrc"
alias sozsh="source ~/.zshrc"

alias path="readlink -f"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_COMPLETION_TRIGGER='@@'
FZF_BAT="--preview='bat {} --style=numbers --color=always --line-range :200'"
export FZF_DEFAULT_OPTS="--extended --height 100% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="fdfind"
export FZF_CTRL_R_OPTS="--extended --height 100% --layout=reverse --border"
export FZF_CTRL_T_OPTS="$FZF_BAT"

function rgf() {
	INITIAL_QUERY=""
	RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case "
	FZF_DEFAULT_COMMAND="$RG_PREFIX '$INITIAL_QUERY'" \
  	fzf --bind "change:reload:$RG_PREFIX {q} || true" \
        --ansi --query "$INITIAL_QUERY" \
        --preview-window 0% | IFS=: read -rA eita

	[ ${eita[1]} ] && echo ${eita[1]}
}

# pacman
alias pmi="sudo pacman -S --needed"
alias pmr="sudo pacman -Rcns"
alias pmu="sudo pacman -Syu"

# apt
if which apt &> /dev/null; then
  pkg="apt"
  if which nala &> /dev/null; then
    pkg="nala"
  fi

  alias apti="sudo $pkg install"
  alias aptud="sudo $pkg update"
  alias aptug="sudo $pkg upgrade"
  alias aptlu="$pkg list --upgradable"
  alias aptr="sudo $pkg remove"
  alias aptar="sudo $pkg autoremove"
  alias apts="$pkg search"
  alias aptadd="sudo add-apt-repository"
fi

# docker
alias dockstart="sudo systemctl start docker"
alias dockstop="sudo systemctl stop docker docker.socket"
alias dockessia="docker start elasticsearch mysql rabbit-mq"

# zsh-autosuggestions
bindkey '^ ' forward-word
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# bpytop (i'm lazy)
alias btop="bpytop"

# zoxide
export PATH="/home/igor/.local/bin:$PATH"
eval "$(zoxide init zsh)"
function zci() {
	z "$(find . -type d | fzf)"
}
alias j="z"
alias jj="z -"
alias ji="zi"
alias jci="zci"

# ffmpeg
alias ffe="ffmpeg -v quiet -stats"
alias ffp="ffplay"

# httpie
function req() {
	http $@ --pretty=all -p="hb" | less
}

# emacs
export PATH=$PATH:~/.emacs.d/bin

# netstat
function ports() {
  netstat -nlpt 2> /dev/null | grep -w "$1"
}

function weather() {
	curl "https://wttr.in/26600000?lang=en" 2> /dev/null | sed -n 3,7p
}



# micro
alias m="micro"

# youtube-dl
alias ydl="youtube-dl"

# bat
export BAT_PAGER="less -R"

export CHROME_EXECUTABLE=/usr/bin/google-chrome-stable

export JAVA_HOME='/usr/lib/jvm/java-11-openjdk-amd64'
export PATH=$JAVA_HOME/bin:$PATH

export PATH=$PATH:/opt/android-sdk
#export ANDROID_SDK_ROOT='/opt/android-sdk'
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools/
export PATH=$PATH:$ANDROID_SDK_ROOT/tools/bin/
export PATH=$PATH:$ANDROID_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/tools/

# golang
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

eval "$(starship init zsh)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/bit bit

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# if [ -z "$VIM_TERMINAL" ]; then
  # VIM_TERMINAL=1 vim -c ":ter"
# fi

colors=( cyan magenta green yellow blue )

# fm6000 -r -c ${colors[$RANDOM % 5 + 1]}

# j core
export DENO_INSTALL="/home/igor/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

export PATH="$PATH:/home/igor/.dotnet"
