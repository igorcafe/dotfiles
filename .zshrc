source ~/.zplug/init.zsh

_omz_register_handler() {}

zplug "plugins/git", from:oh-my-zsh
zplug "plugins/colored-man-pages", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "themes/robbyrussell", from:oh-my-zsh
zplug "junegunn/fzf", from:gh-r, as:command, use:"*darwin*amd64"
# zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting"

zplug load

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(direnv hook zsh)"
export PATH="$PATH:$(go env GOPATH)/bin"

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"

. "$HOME/.local/bin/env"

export CHROME_EXECUTABLE="google-chrome-stable"
export EDITOR="emacsclient -c -r"
