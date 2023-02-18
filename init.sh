#!/bin/bash

wd="$(pwd)"

dotfiles=(
	.config/starship.toml
	.zshrc
)

for file in ${dotfiles[@]}; do
	ln -s "$wd/$file" ~/"$file"
done
