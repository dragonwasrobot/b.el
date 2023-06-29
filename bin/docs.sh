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
     _
    | |
  __| | ___   ___ ___
 / _` |/ _ \ / __/ __|
| (_| | (_) | (__\__ \\
 \__,_|\___/ \___|___/

────────────────────────────────────────────────────────────────────────────────
Required packages present:
 ETD: $etd
Dash: $dash
════════════════════════════════════════════════════════════════════════════════
$(emacs --version)
════════════════════════════════════════════════════════════════════════════════
INFO

emacs --batch \
  -eval "(add-to-list 'load-path \".\")" \
  -eval "(add-to-list 'load-path \"${etd}\")" \
  -eval "(add-to-list 'load-path \"${dash}\")" \
  -l ert \
  -l etd \
  -l ./b.el \
  -l ./dev/b-examples.el \
  --eval "(message \"Update README.md\")" \
  --eval "(etd-create-docs-file-for \"dev/b-examples.el\" \"dev/doc-template\" \"README.md\")" \
  --eval "(message \"Update index.md\")" \
  --eval "(etd-create-docs-file-for \"dev/b-examples.el\" \"dev/index-template\" \"index.md\")"
