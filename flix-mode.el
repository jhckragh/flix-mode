;;; flix-mode.el --- Major mode for Flix code -*- lexical-binding: t; -*-

;; Copyright (c) 2021  Jacob Harris Cryer Kragh

;; Author: Jacob Harris Cryer Kragh <jhckragh@gmail.com>
;; Version: 0.0.3
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides rudimentary font-lock (syntax highlighting) for the Flix
;; programming language (https://flix.dev).

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

;;; Code:

(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

(defconst flix-mode-keywords
  '("as" "alias" "and" "case" "chan" "choose" "class" "def" "deref" "else"
    "enum" "false" "forall" "force" "from" "get" "if" "import" "instance"
    "inline" "into" "lat" "law" "let" "lawless" "lazy" "main" "match"
    "namespace" "null" "not" "new" "opaque" "or" "override" "project"
    "pub" "Pure" "query" "Record" "ref" "rel" "Schema" "sealed" "select"
    "set" "solve" "spawn" "true" "type" "unlawful" "use" "with" "where")
  "Keywords recognized by `flix-mode'.")

(defvar flix-mode-font-lock-keywords
  `(("\\_<\\(Impure\\)\\_>" (0 font-lock-warning-face))
    ("\\_<\\(Pure\\)\\_>" (0 font-lock-function-name-face))
    ("def +\\([_[:lower:]][_[:alnum:]]*\\)" (1 font-lock-function-name-face))
    ("\\_<\\([_[:lower:]][_[:alnum:]]*\\) *:" (1 font-lock-variable-name-face))
    ("\\_<\\([_[:upper:]][_[:alnum:]]*\\)\\_>" (0 font-lock-type-face))
    ("let +\\([_[:lower:]][_[:alnum:]]*\\)" (1 font-lock-variable-name-face))
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

;; Indentation heuristics (BEGIN)

;; TODO: Some of the regexps will break in the presence of certain kinds of comments.
;; TODO: Handle multi-line function calls, lambdas, pipe chains etc.

(defun flix-mode--skip-syntax-forward ()
  (skip-syntax-forward " ")
  (when (looking-at "/\\(/\\|\\*\\)")
    (forward-char 2)))

(defun flix-mode--point-inside-comment-p ()
  (nth 4 (syntax-ppss)))

(defun flix-mode--point-inside-string-p ()
  (nth 3 (syntax-ppss)))

(defun flix-mode--re-search-backward (regexp)
  "Works the same as `re-search-backward' except matches inside
comments and strings are ignored."
  (catch 'break
    (while t
      (re-search-backward regexp)
      (when (and (not (flix-mode--point-inside-comment-p))
                 (not (flix-mode--point-inside-string-p)))
        (throw 'break nil)))))

(defun flix-mode--goto-first-nonblank-line-above ()
  (catch 'break
    (while (not (bobp))
      (forward-line -1)
      (flix-mode--skip-syntax-forward)
      (let ((line (thing-at-point 'line t)))
        (when (and (not (string-match-p "\\`[ \t\n\r]*\\'" line))
                   (not (flix-mode--point-inside-comment-p)))
          (throw 'break nil))))))

(defun flix-mode--brace-starts-enclosing-block-p (line-number)
  (save-excursion
    (condition-case nil
        (progn
          (forward-sexp)
          (< line-number (line-number-at-pos)))
      (error t))))

(defun flix-mode--indent-closing-brace-line ()
  (let ((indent 0))
    (save-excursion
      (end-of-line)
      (ignore-errors
        (backward-sexp)
        (setq indent (current-indentation))))
    (indent-line-to indent)))

(defun flix-mode--indent-decl-line ()
  (let ((indent 0)
        (line-number (line-number-at-pos)))
    (save-excursion
      (catch 'break
        (while t
          (condition-case nil
              (flix-mode--re-search-backward "{")
            (error
             (throw 'break nil)))
          (when (flix-mode--brace-starts-enclosing-block-p line-number)
            (setq indent (+ (current-indentation) tab-width))
            (throw 'break nil)))))
    (indent-line-to indent)))

(defun flix-mode--indent-else-line ()
  (let ((indent 0))
    (save-excursion
      (ignore-errors
        (flix-mode--re-search-backward "\\_<if\\_>")
        (setq indent (current-indentation))))
    (indent-line-to indent)))

(defun flix-mode--indent-case-line ()
  (let ((indent 0))
    (save-excursion
      (flix-mode--goto-first-nonblank-line-above)
      (if (string-match-p "\\_<def\\_>\\|{ *$" (thing-at-point 'line t))
          (setq indent (+ (current-indentation) tab-width))
        (end-of-line)
        (ignore-errors
          (flix-mode--re-search-backward "\\_<case\\_>")
          (setq indent (current-indentation)))))
    (indent-line-to indent)))

(defun flix-mode--indent-nondecl-line ()
  (let ((indent 0)
        (line-number (line-number-at-pos)))
    (save-excursion
      (flix-mode--goto-first-nonblank-line-above)
      (let ((neighbor (thing-at-point 'line)))
        (cond
         ((string-match-p "[,;\\.] *$" neighbor)
          (setq indent (current-indentation)))
         ((or (string-match-p "\\_<\\(def\\|if\\|else\\|case\\)\\_>" neighbor)
              (string-match-p "{ *$" neighbor))
          (setq indent (+ (current-indentation) tab-width))))))
    (indent-line-to indent)))

(defun flix-mode-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((line (thing-at-point 'line t)))
      (cond
       ((string-match-p "^ *}" line)
        (flix-mode--indent-closing-brace-line))
       ((string-match-p "\\_<\\(def\\|enum\\|type\\|namespace\\|rel\\)\\_>" line)
        (flix-mode--indent-decl-line))
       ((string-match-p "\\_<case\\_>" line)
        (flix-mode--indent-case-line))
       ((string-match-p "^ *else" line)
        (flix-mode--indent-else-line))
       (t
        (flix-mode--indent-nondecl-line))))))

;; Indentation heuristics (END)

(define-derived-mode flix-mode prog-mode "Flix"
  "A major mode for editing Flix files."
  :syntax-table flix-mode-syntax-table
  (setq-local comment-start "//")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\) *")
  (setq-local comment-end "")
  (setq-local indent-line-function 'flix-mode-indent-line)
  (setq-local font-lock-defaults '(flix-mode-font-lock-keywords)))

(provide 'flix-mode)
