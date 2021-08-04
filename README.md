# Flix Mode

[![Version](https://img.shields.io/badge/version-0.0.8-green)]()

Unofficial Emacs major mode for the [Flix programming language](https://flix.dev).

<img width="400" src="screenshot.png">

*Features:*

* Syntax highlighting.
* Rudimentary indentation support (work in progress).

## Installation

Download `flix-mode.el` and then do the following in Emacs:

<kbd>M-x package-install-file [RET] /path/to/flix-mode.el [RET]</kbd>

The built-in package manager in Emacs will then install the mode.

The mode is automatically loaded when you visit a `.flix` file.

## Known limitations

* The indentation support is pretty crude and not nearly as automatic as in other major modes.
* As in many other major modes, the syntax highlighting is done via regular expressions; so strange source layout can break it.

## Contributing

Contributions are very welcome! If you're unfamiliar with Emacs Lisp, then 
the following resources are recommended reading:

* https://github.com/chrisdone/elisp-guide
* https://www.emacswiki.org/emacs/ModeTutorial
* https://www.emacswiki.org/emacs/SampleMode

## License

This project is distributed under the GNU General Public License, which can
be viewed by typing <kbd>C-h C-c</kbd> in Emacs.
