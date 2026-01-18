#!/usr/bin/env bash

gnome-extensions list --user --enabled | sort | diff extensions.txt -
