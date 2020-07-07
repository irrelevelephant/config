#!/bin/bash

BRIGHTNESS=$(xrandr --verbose | grep -i brightness | cut -f2 -d ' ' | head -n 1)

if [ "$1" = "up" ]; then
    BRIGHTNESS=$(echo "if ($BRIGHTNESS + 0.1 > 1.0) 1.0 else $BRIGHTNESS + 0.1" | bc -l)
elif [ "$1" = "down" ]; then
    BRIGHTNESS=$(echo "if ($BRIGHTNESS - 0.1 < 0.0) 0.0 else $BRIGHTNESS - 0.1" | bc -l)
fi

xrandr --output eDP --brightness $BRIGHTNESS
