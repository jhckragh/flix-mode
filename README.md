# Flix Mode

[![Version](https://img.shields.io/badge/version-0.0.3-green)]()

(Unofficial) Emacs major mode that provides rudimentary syntax
highlighting and indentation for the [Flix programming language](https://flix.dev).

<div align="center">
<img width="400" src="screenshot.png">
</div>

## Installation

Download `flix-mode.el` to a directory of your choice and then instruct
Emacs to load it, e.g., by adding the following to your `.emacs` file:

```elisp
(load "/path/where/you/put/it/flix-mode")
```

After restarting Emacs, the mode should then automatically be enabled
when you visit a `.flix` file.

## Known limitations

The indentation heuristics are very much work in progress. They should mostly work for code that follows the style used in the official Flix examples. But multi-line function calls, lambdas, pipes etc. aren't handled, and placing comments in certain special places can cause the heuristics to indent incorrectly.

## Contributing

Contributions are very welcome! If you're unfamiliar with Emacs Lisp, then 
the following resources are recommended reading:

* https://github.com/chrisdone/elisp-guide
* https://www.emacswiki.org/emacs/ModeTutorial
* https://www.emacswiki.org/emacs/SampleMode

## License

This project is distributed under the GNU General Public License, which can
be viewed by typing <kbd>C-h C-c</kbd> in Emacs.
