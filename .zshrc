source ~/.zplug/init.zsh

_omz_register_handler() {}

zplug "plugins/git", from:oh-my-zsh
zplug "plugins/colored-man-pages", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "themes/robbyrussell", from:oh-my-zsh
zplug "junegunn/fzf", from:gh-r, as:command, use:"*darwin*amd64"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting"

zplug load

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

. /opt/homebrew/opt/asdf/libexec/asdf.sh

export PATH="$PATH:$(go env GOPATH)/bin"
