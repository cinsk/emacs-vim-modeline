emacs-vim-modeline
==================

Handle VIM modeline in Emacs.

Like Emacs's file local variable, VIM (and others) has similar feature called `modeline` (Unfortunately, VIM uses `modeline` for its own purpose, which is very different from Emacs's `modeline`.)  For example:

    /* vim: set nu: */
    // vi:nu:sw=3

This package provides basic support for handling VIM modeline in a file.

Currently, VIM's ``shiftwidth``, ``tabstop``, ``softtabstop``, ``number``, ``expandtab`` options are supported (in somewhat limited ways).

Install
-------

    $ # Download emacs-vim-modeline in someplace.
    $ git clone http://github.com/cinsk/emacs-vim-modeline.git

In your init file, add following code:

    (add-to-path 'load-path "SOMEWHERE/emacs-vim-modeline")
    (require 'vim-modeline)
    (add-to-list 'find-file-hook 'vim-modeline/do)

