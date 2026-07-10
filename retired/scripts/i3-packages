#!/bin/bash

packages=$(($(apt list --upgradable | wc -l) - 1))

if [ "$packages" != "0" ] ; then
    echo "📦 $packages"
fi
