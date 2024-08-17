#!/bin/bash

if [ "$#" -ne 1 ] || { [ "$1" != "up" ] && [ "$1" != "down" ]; }; then
    echo "Usage: $0 {up|down}"
    exit 1
fi

declare -A paths=(
    ["~/.emacs.d/user/*.el"]="./.emacs.d/user/"

    ["~/.config/gtk-3.0/"]="./.config/gtk-3.0/"
    ["~/.config/kitty/"]="./.config/kitty/"
    ["~/.config/swap/"]="./.config/sway/"
    ["~/.config/waybar/"]="./.config/waybar/"
    ["~/.config/wofi/"]="./.config/wofi/"

    ["~/.bashrc"]="./.bashrc"
    ["~/.mg"]="./.mg"
    ["~/.gitconfig"]="./.gitconfig"

)

sync_direction="$1"

for src in "${!paths[@]}"; do
    src_expanded=$(eval echo "$src")
    dest_expanded=$(eval echo "${paths[$src]}")

    if [ "$sync_direction" == "up" ]; then
        for file in $src_expanded; do
            echo "Syncing $file to $dest_expanded (UP)"
            rsync -av --progress "$file" "$dest_expanded"
        done
    else
        for file in $dest_expanded; do
            echo "Syncing $file to $src_expanded (DOWN)"
            rsync -av --progress "$file" "$src_expanded"
        done
    fi
done

echo "Sync completed."
