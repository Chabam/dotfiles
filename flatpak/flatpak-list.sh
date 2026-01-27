#!/usr/bin/env bash

flatpak list --app --columns=ref | tail -n +2 > flatpaks.txt
