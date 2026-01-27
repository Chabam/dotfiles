#!/usr/bin/env bash

awk '{ system("flatpak install "$1) }' flatpaks.txt
