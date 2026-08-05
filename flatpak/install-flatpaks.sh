#!/usr/bin/env bash

awk '{ system("flatpak --$2 install "$1) }' flatpaks.txt
