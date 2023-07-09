#!/bin/sh

etd="$(dirname "$(find ~/.emacs.d | grep "/etd.el$" | tail -1)")"
dash="$(dirname "$(find ~/.emacs.d | grep "/dash.el$" | tail -1)")"

cat <<INFO
════════════════════════════════════════════════════════════════════════════════
 _          _
| |        | |
| |__   ___| |
| '_ \ / _ \ |
| |_) |  __/ |
|_.__(_)___|_|
 _            _
| |          | |
| |_ ___  ___| |_ ___
| __/ _ \/ __| __/ __|
| ||  __/\__ \ |_\__ \\
 \__\___||___/\__|___/

────────────────────────────────────────────────────────────────────────────────
Required packages present:
ETD: $etd
Dash: $dash
════════════════════════════════════════════════════════════════════════════════
$(emacs --version)
════════════════════════════════════════════════════════════════════════════════
INFO

emacs --batch \
  -eval "(add-to-list 'load-path \"${dash}\")" \
  -eval "(add-to-list 'load-path \"${etd}\")" \
  -eval "(add-to-list 'load-path \".\")" \
  -l ert \
  -l dash \
  -l etd \
  -l ./b.el \
  -l ./dev/b-examples.el \
  --eval "(ert-run-tests-batch-and-exit)"
exitcode=$?

echo "
════════════════════════════════════════════════════════════════════════════════"
exit $exitcode
