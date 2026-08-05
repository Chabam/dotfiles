#!/usr/bin/env bash

flatpak list --app --columns=ref,installation | tail -n +1 > flatpaks.txt
