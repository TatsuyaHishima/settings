#!/usr/bin/zsh

export name=$1
platex $1
dvipdfmx ${name%.*}