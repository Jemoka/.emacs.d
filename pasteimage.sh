#!/usr/bin/env bash

folder=$(pwd)
filename=$(date +%Y-%m-%d\ at\ %H.%M.%S).png

if [ $# -ne 0 ]; then
    if [ -d "$1" ]; then
        if [ "$1" != "." ]; then folder="$1"; fi
    else
        a=$(dirname "$1")
        b=$(basename "$1" )

        if [ "$b" != "" ]; then filename="$b"; fi

        if [ "$a" != "." ]; then folder="$a"; fi
    fi
fi

osascript -e "tell application \"System Events\" to ¬
        write (the clipboard as «class PNGf») to ¬
        (make new file at folder \"$folder\" ¬
        with properties {name:\"$filename\"})"
