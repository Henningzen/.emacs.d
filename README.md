# .emacs.d

My Emacs settings - updated 2025


## Introduction

I inherited a set of brilliant Emacs settings from [Magnar Sveen](https://github.com/magnars) in 2019.
That setup was geared towards Mac and Norwegian keyboard layout, and I tweaked
parts of it towards US International keys and Linux.

In early 2025 the project with .emacs-d was rebooted when I decided I wanted to
learn more about what goes into the configuration, and realise that I am most
likely stuck with Emacs for life, due to Clojure being my main focus and
Org-mode is my primary way of organizing myself.

This second time over I have also consulted and copied from [Christian Johansen](https://github.com/cjohansen)
.emacs.d repo, according to what has been stated in the [README.md](https://github.com/cjohansen/.emacs.d/blob/master/README.md)

>"I recommend starting with a blank emacs + Technomancy's better-defaults
>package, and then dig through this repo for useful nuggets, instead of
>forking it directly."


Feel free to clone or copy. Emacs is most recommendable, even though it can be
very intrusive and dangerously addictive.

## Setup

The first time you start emacs, it will install some additional packages
that are best handled by the package manager.

* browse-kill-ring
* change-inner
* diminish
* dash
* datomic-snippets
* expand-region
* find-file-in-project
* jump-char
* multifiles
* smart-forward
* smex
* tagedit

In addition, make sure you have:

* cloned [Technomancy's better-defaults](https://git.sr.ht/~technomancy/better-defaults)
* Created a custom.el file


## Install emacs on Linux

I have installed Emacs from source, using this guide [Installing Emacs 29.1 on Ubuntu 22.04 LTS](https://arnesonium.com/2023/07/emacs-29-1-on-ubuntu-22-04-lts)
by Erik L. Arneson.


## Using these emacs settings

* Quit emacs with `C-x r q`, mnemonic *Really Quit* is easier that Googling how to quit Vim.

 * Find file in project with `C-x o`, in dir with `C-x C-f`, recent with `C-x f`

 * Add your user- and project-specific stuff in .emacs.d/users/[machine name]/*.el

 * `C-h` is rebound to backspace, like in the shell. Get help on `F1` instead.

 * Autocomplete with `C-.` (autocomplete entire lines with `C-:`)

 * expand-region: Find its bound key by doing `F1 f er/expand-region`

 * Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * ~~Quickly jump anywhere in the buffer with `C-ø` then the starting letter of a word.~~

 * Indent and clean up white space in the entire buffer with `C-c n`

 * We recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

## Using Emacs

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (bound to cmd on my mac settings)
* `S      ` Shorthand for the shift-key

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x o  ` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x C-j` Jump to this files' current directory
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills
* `C-@    ` Mark stuff quickly. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-x m  ` Open magit. It's a magical git interface for emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically
* `S-arrow` Jump to window to the left/right/up/down

### Help

* `F1 t   ` Basic tutorial
* `F1 k   ` Help for a keybinding
* `F1 r   ` Emacs' extensive documentation

### Advanced usage
* `C-c j e j` copy-edn-as-json
* `C-c j j e` copy-json-as-edn

## TODO
* Warning (comp): perspective.el:2219:6: Warning: ‘ibuffer-awhen’ is an obsolete macro (as of 29.1); use ‘when-let’ instead.
* Warning (comp): paredit.el:714:33: Warning: ‘point-at-eol’ is an obsolete function (as of 29.1); use ‘line-end-position’ or ‘pos-eol’ instead.
* Warning (comp): paredit.el:723:42: Warning: ‘point-at-bol’ is an obsolete function (as of 29.1); use ‘line-beginning-position’ or ‘pos-bol’ instead.
* Warning (comp): datomic-snippets.el:87:48: Warning: reference to free variable ‘datomic-snippets-root’
* Warning (comp): datomic-snippets.el:90:6: Warning: ‘yas/load-directory’ is an obsolete function (as of yasnippet 0.8); use ‘yas-load-directory’ instead.
