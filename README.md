# Duplexer

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/duplexer-badge.svg)](http://melpa.org/#/duplexer)

Handle conflicts between local minor modes and reuse rules.

This package provides method to handle conflicts between local minor modes and reuse rules.
When a CALLER mode enable, call related CALLEE modes with arguments and restore them
automatically after the CALLER mode disabled.

<!-- markdown-toc start -->

## Contents

- [Duplexer](#duplexer)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Limit](#limit)
  - [License](#license)

<!-- markdown-toc end -->

## Install

### dependencies

- emacs, version >= 29.1
- dash

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA][melpa].
Install with `M-x package-install` <kbd>RET</kbd> `duplexer` from within Emacs.

## Usage

- `duplexer-alist`

Every item in `duplexer-alist` is in format of (CALLER . ((CALLEE ARG) GROUP ...)).
CALLER is a local minor mode.
CALLEE should be a local minor mode. ARG is 1 or -1.
Instead of list, a symbol GROUP which defined in `duplexer-groups` is allowd either.

- `duplexer-groups`

Every item in `duplexer-groups` is in format of (GROUP . ((CALLEE ARG) ...)).
GROUP is a collection of rules, which could be used repeatly in `duplexer-alist`

- `duplexer-fallback-alist`

Some minor MODE have different variable instead of var MODE to store the state informations. For example, `read-only-mode` has the buffer-local var `buffer-read-only` and `save-place-local-mode` has the buffer-local var `save-place-mode`. Add these special cases to `duplexer-fallback-alist` if you meet such conditions.

```emacs-lisp
;; by default
(defcustom duplexer-fallback-alist
  '((read-only-mode . buffer-read-only)
    (save-place-local-mode . save-place-mode))
  "Alist of element (MINOR-MODE . MIOR-MODE-VARIABLE).
Some minor modes use another variable instead of MODE to store the state of the
mode.  Add these cases here as fallback."
  :type '(alist :key-type (symbol :tag "Minor mode name")
                :value-type (symbol :tag "Minor mode variable"))
  :group 'duplexer)
```

- `duplexer-force`

if non-nil, apply rule by force even a contrary rule has been applied in the current buffer already.

Full example:

```emacs-lisp
(require 'duplexer)
(duplexer-mode)

;; simple usage
(setq duplexer-alist
      '(;; disable blink-cursor-mode and enable read-only-mode when turn on olivetti-mode
        (olivetti-mode . ((blink-cursor-mode -1)
                          (read-only-mode 1)))
        ;; disable highlight-parentheses-mode and save-place-local-mode when turn on smerge-mode
        (smerge-mode   . ((highlight-parentheses-mode -1)
                          (save-place-local-mode -1)))))
;; or more advanced
(setq duplexer-groups
      '(;; built-in
        (hl-line . ((hl-line-mode -1)
                    (global-hl-line-mode -1)))
        (read-only . ((read-only-mode 1)))
        (cursor    . ((blink-cursor-mode -1)))
        ;; third-party
        (mode-line . ((hide-mode-line-mode 1)))
        (hide-margin . ((diff-hl-margin-local-mode -1)
                        (binky-margin-local-mode -1)))
        (paren . ((highlight-parentheses-mode -1)))))

;; use symbols to extract group rules
(setq duplexer-alist
      '((olivetti-mode  . (margin mode-line (focus-mode -1)))
        (redacted-mode  . (margin hl-line cursor mode-line paren read-only))
        (smerge-mode    . (margin paren (save-place-local-mode -1)))))
```

## Limit

`duplexer-mode` is designed for local minor modes, however, there are some cases which
global-minor-mode must be called.

- Such as `hl-line-mode`, if `global-hl-line-mode` is enable, call `(hl-line-mode -1)` alone would not take effect in current buffer.
- Such as `blink-cursor-mode`, some ui-related modes have no local minor mode at all.

Be careful when calling global minor mode for which will bother other buffers globally.

## License

See [LICENSE](LICENSE).

[melpa]: http://melpa.org/#/duplexer
