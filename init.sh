#!/bin/bash

wd="$(pwd)"

FORCE=0
if [[ "$1" == "-f" ]]; then
	FORCE=1
fi

function cmd () {
	echo -e "> \033[1;30m$@\033[0m"
	eval "($@)"
	echo
}

for d in $(find .config -type d); do
	cmd mkdir -p ~/"$d"
done

files=(
	.zshrc
	$(find .config -type f)
	$(find .local -type f)
)

for file in ${files[@]}; do
	if [ $FORCE -eq 1 ]; then
		cmd rm ~/"$file" 2> /dev/null
	fi
	cmd ln -s "$wd/$file" ~/"$file"
done
