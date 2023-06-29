#!/bin/sh -e

# For CI / Github Actions

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="etd dash"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (package-refresh-contents) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -batch \
         --eval "$INIT_PACKAGE_EL"
