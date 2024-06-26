export ZSH="/home/igor/.oh-my-zsh"

ZSH_THEME="half-life"
CASE_SENSITIVE="true"
DISABLE_MAGIC_FUNCTIONS="true"

plugins=(
  git
  colored-man-pages
  sudo
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# configuring highlight
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[builtin]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[command]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[alias]="fg=cyan,bold"
# ZSH_HIGHLIGHT_STYLES[single-hyphen-option]="fg=black,bold"
# ZSH_HIGHLIGHT_STYLES[double-hyphen-option]="fg=black,bold"

source $ZSH/oh-my-zsh.sh

# custom git aliases and stuff
alias gdc="git diff --cached"
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
alias glff="git pull --ff-only"

function grco() {
  res="$(grf | grep checkout: | egrep -o "to .*" | cut -d' ' -f2 | awk '!x[$0]++' | fzf)"
  [ $? -eq 0 ] && gco "$res"
}

# lsd
alias ls="lsd"
alias l="lsd"
alias la="lsd -a"
alias ll="lsd -l"
alias lal="lsd -al"

# fzf
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_COMPLETION_TRIGGER='@@'
FZF_BAT="--preview='bat {} --style=numbers --color=always --line-range :200'"
export FZF_DEFAULT_OPTS="--extended --height 100% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="fdfind"
export FZF_CTRL_R_OPTS="--extended --height 100% --layout=reverse --border"
export FZF_CTRL_T_OPTS="$FZF_BAT"

# zsh-autosuggestions
bindkey '^ ' forward-word
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# zoxide
eval "$(zoxide init zsh)"
alias j="z"
alias jj="z -"
alias ji="zi"
alias jci="zci"

# bat
export BAT_PAGER="less -R"

# ffmpeg (TODO: use profiles)
alias ffmpeg="ffmpeg -hide_banner -loglevel error -stats"

# PATH
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.nimble/bin"
export PATH="$PATH:/opt/flutter/bin"

CHROME_EXECUTABLE=$(which google-chrome-stable)

#eval "$(starship init zsh)"

function gotemp () {
	cd /tmp
	mkdir -p gotemp
	cd gotemp
	go mod init gotemp 2> /dev/null
	if ! [ -f main.go ]; then
		echo "package main\n\nfunc main() {\n\n}\n" > main.go
	fi
}

#source /usr/share/nvm/init-nvm.sh

if [ -e /home/igor/.nix-profile/etc/profile.d/nix.sh ]; then . /home/igor/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
