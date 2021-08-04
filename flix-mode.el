;;; flix-mode.el --- Major mode for Flix code -*- lexical-binding: t; -*-

;; Copyright (c) 2021  Jacob Harris Cryer Kragh

;; Author: Jacob Harris Cryer Kragh <jhckragh@gmail.com>
;; Version: 0.0.8
;; Keywords: languages
;; URL: https://github.com/jhckragh/flix-mode

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unofficial major mode for the Flix programming language (https://flix.dev).
;; Provides rudimentary syntax highlighting and some indentation support.

;;; Code:

(defconst flix-mode-keywords
  '("as" "alias" "and" "case" "chan" "choose" "class" "def" "deref" "else"
    "enum" "forall" "force" "from" "get" "if" "import" "instance"
    "inline" "into" "lat" "law" "let" "lawless" "lazy" "main" "match"
    "namespace" "null" "not" "new" "opaque" "or" "override" "project"
    "pub" "Pure" "query" "Record" "ref" "rel" "Schema" "sealed" "select"
    "set" "solve" "spawn" "type" "unlawful" "use" "with" "where")
  "Keywords recognized by `flix-mode'.")

(defvar flix-mode-font-lock-keywords
  `(("\\_<Impure\\_>" (0 font-lock-warning-face))
    ("\\_<Pure\\_>" (0 font-lock-function-name-face))
    ("\\_<\\(true\\|false\\)\\_>" (0 font-lock-builtin-face))
    ("def[ \t]+\\([_[:lower:]][_[:alnum:]]*\\)" (1 font-lock-function-name-face))
    ("\\_<\\([_[:lower:]][_[:alnum:]]*\\) *:" (1 font-lock-variable-name-face))
    ("\\_<\\([_[:upper:]][_[:alnum:]]*\\)\\_>" (0 font-lock-type-face))
    ("let[ \t]+\\([_[:lower:]][_[:alnum:]]*\\)" (1 font-lock-variable-name-face))
    (,(concat "\\_<" (regexp-opt flix-mode-keywords) "\\_>") (0 font-lock-keyword-face)))
  "Keyword highlighting for `flix-mode'.")

(defvar flix-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `flix-mode'.")

(defcustom flix-mode-tab-width 4
  "The tab width to use for indentation.")

(defun flix-mode--indent-further ()
  (interactive)
  (let ((new-indent (+ (current-indentation) flix-mode-tab-width)))
    (if (> (current-column) (current-indentation))
        (save-excursion (indent-line-to new-indent))
      (indent-line-to new-indent))))

(defun flix-mode--indent-less ()
  (interactive)
  (save-excursion
    (indent-line-to (max 0 (- (current-indentation) flix-mode-tab-width)))))

(defun flix-mode--newline-and-maybe-indent ()
  (interactive)
  (let ((indent-further (and (eolp) (looking-back "[{(=]")))
        (prev-indent (current-indentation)))
    (newline)
    (if indent-further
        (indent-line-to (+ prev-indent flix-mode-tab-width))
      (indent-line-to prev-indent))))

(defvar flix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'flix-mode--newline-and-maybe-indent)
    (define-key map [return] 'flix-mode--newline-and-maybe-indent)
    (define-key map "\C-j" 'flix-mode--newline-and-maybe-indent)
    (define-key map [tab] 'flix-mode--indent-further)
    (define-key map [backtab] 'flix-mode--indent-less)
    map)
  "Keymap for `flix-mode'.")

;;;###autoload
(define-derived-mode flix-mode prog-mode "Flix"
  "A major mode for editing Flix files."
  :syntax-table flix-mode-syntax-table
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "//")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\) *")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(flix-mode-font-lock-keywords))
  (add-to-list 'electric-indent-chars ?\}))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

(provide 'flix-mode)

;;; flix-mode.el ends here
