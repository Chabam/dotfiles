#!/bin/sh
# org.freedesktop.appearance color-scheme
#
# Indicates the system's preferred color scheme.
# Supported values are:
#
#   0: No preference
#   1: Prefer dark appearance
#   2: Prefer light appearance
#
# Unknown values should be treated as 0 (no preference).

scheme=$(
  gdbus call --session --timeout=1000 \
             --dest=org.freedesktop.portal.Desktop \
             --object-path /org/freedesktop/portal/desktop \
             --method org.freedesktop.portal.Settings.Read org.freedesktop.appearance color-scheme
)

case $scheme in
  ( '(<<uint32 1>>,)' ) exit 1;;
  ( '(<<uint32 2>>,)' ) exit 2;;
  ( *                 ) exit 0;;
esac
