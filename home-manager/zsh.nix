{...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    shellAliases = {
      updatesys = "sudo nixos-rebuild switch --flake ~/Git/dotfiles";
      updatehome = "home-manager switch -b backup --flake ~/Git/dotfiles";
      ffmpeg = "ffmpeg -hide_banner -loglevel error -stats";
      ffplay = "ffplay -hide_banner";
      ffprobe = "ffprobe -hide_banner";
      ls = "lsd";
      ll = "lsd -l";
      lla = "lsd -la";
      j = "z";
    };
    zplug = {
      enable = true;
      plugins = [
        {
          name = "plugins/git";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "plugins/colored-man-pages";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "plugins/sudo";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "themes/robbyrussell";
          tags = [ "from:oh-my-zsh" ];
        }
        { name = "zsh-users/zsh-autosuggestions"; }
        { name = "zsh-users/zsh-syntax-highlighting"; }
      ];
    };

    initExtra = ''
    bindkey "^[[1;5C" forward-word
    bindkey "^[[1;5D" backward-word

    gotemp () {
      d=/tmp/gotemp-$RANDOM
      mkdir $d && cd $d && echo "package main\n\nfunc main() {\n\t\n}\n" > main.go
    }

    function gcl2() {
      ownerrepo=$(echo "$1" | sed -E 's/((https?:\/\/)|(git@))[^:/]+.//;s/\.git$//;s/([^/]+)\/([^/]+).*/\1\/\2/')
      onwer=$(echo $ownerrepo | cut -d'/' -f1)
      repo=$(echo $ownerrepo | cut -d'/' -f2)
      dest=~/git/$owner/$repo
      mkdir -p $dest
      git clone --recurse-submodules "$1" "$dest"
      cd "$dest"
    }

    eval "$(zoxide init zsh)"
    '';
  };
}
