#!/bin/bash

if [ "$#" -ne 1 ] || { [ "$1" != "up" ] && [ "$1" != "down" ]; }; then
    echo "Usage: $0 {up|down}"
    exit 1
fi

declare -A paths=(
    ["~/.emacs.d/user/*.el"]="./.emacs.d/user/"

    ["~/.config/gtk-3.0/*"]="./.config/gtk-3.0/"
    ["~/.config/kitty/*"]="./.config/kitty/"
    ["~/.config/sway/*"]="./.config/sway/"
    ["~/.config/waybar/*"]="./.config/waybar/"
    ["~/.config/wofi/*"]="./.config/wofi/"

    ["~/.bashrc"]="./.bashrc"
    ["~/.mg"]="./.mg"
    ["~/.gitconfig"]="./.gitconfig"
)

sync_direction="$1"

for src in "${!paths[@]}"; do
    src_expanded=$(eval echo "$src")
    dest_expanded=$(eval echo "${paths[$src]}")

    # don't sync monitor config
    if [[ "$src_expanded" == *"monitors.conf" ]]; then
        if [ "$sync_direction" == "up" ]; then
            echo "Creating empty monitors.conf in $dest_expanded (UP)"
            > "./.config/sway/monitors.conf"
        fi
        continue
    fi

    if [ "$sync_direction" == "up" ]; then
        if [[ "$dest_expanded" == */ ]]; then
            mkdir -p "$dest_expanded"
        fi
        echo "Syncing $src_expanded to $dest_expanded (UP)"
        cp -r $src_expanded "$dest_expanded"
    else
        if [[ "$src_expanded" == */ ]]; then
            mkdir -p "$(dirname "$src_expanded")"
        fi
        echo "Syncing $dest_expanded to $src_expanded (DOWN)"
        cp -r $dest_expanded "$(dirname "$src_expanded")"
    fi
done

echo "Sync completed."
