;;; flex-mode ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/flex-mode
;; Package-Requires: 
;; Created: 17 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; [![Build Status](https://travis-ci.org/nverno/flex-mode.svg?branch=master)](https://travis-ci.org/nverno/flex-mode)

;;; Code:

(eval-when-compile
  (require 'derived))
(require 'cc-mode)
(autoload 'flex-src-edit "flex-src")

(defgroup flex nil
  "Major mode for flex files."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages
  :prefix "flex-")

(defcustom flex-indent-level 4
  "Amount by which expressions are indented."
  :type 'integer
  :group 'flex)

;; -------------------------------------------------------------------

(defvar flex-decls-opener "%{")
(defvar flex-decls-closer "%}")
(defvar flex-grammar-delim "%%")

(defalias 'flex-indent-command #'c-indent-command)

;;——— Commands ———————————————————————————————————————————————————————

(defun flex-compile ()
  (interactive)
  (let* ((base (file-name-nondirectory
                (file-name-sans-extension
                 buffer-file-name)))
         (compile-command
          (format
           (eval-when-compile
             (concat "flex %s && "
                     "g++ -g -Wall -Wno-unused -Wno-write-strings "
                     "-o %s%s lex.yy.c -lfl"))
           buffer-file-name base
           (if (eq system-type 'windows-nt) ".exe" ".out")))
         (compilation-read-command nil))
    (call-interactively 'compile)))

;; Compile and run on test file.
(defun flex-compile-run ()
  (interactive))

;; Align declarations in region.
(defun flex-align-region-dwim (&optional _arg)
  (interactive))

;;; Navigate

;; jump to next section
(defun flex-next-section ()
  (interactive)
  (re-search-forward "%[%}{]" nil 'move))

(defun flex-previous-section ()
  (interactive)
  (re-search-backward "%[%}{]" nil 'move))

;;——— Major Mode —————————————————————————————————————————————————————

;; Menu
(defvar flex-menu
  '("Flex"
    ["Compile" flex-compile t]
    ["Edit" flex-src-edit t]
    ["Align Declarations" flex-align-decls t]
    ["Compile and Run" flex-compile-and-run t]))

;; Map
(defvar flex-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil flex-menu)
    (define-key km (kbd "C-c '") #'flex-src-edit)
    (define-key km (kbd "M-N")   #'flex-next-section)
    (define-key km (kbd "M-P")   #'flex-previous-section)
    (define-key km (kbd "<f5>")  #'flex-compile)
    (define-key km "{"           #'self-insert-command)
    (define-key km "}"           #'self-insert-command)
    (define-key km (kbd "TAB")   #'flex-indent-command)
    km))

;;;###autoload
(define-derived-mode flex-mode c-mode "Flex"
  "Major mode for editing flex files derived from `c-mode'.\n
\\{flex-mode-map}"
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  ;; Indentation
  (make-local-variable 'c-basic-offset)
  (make-local-variable 'c-offsets-alist)
  (setq-local indent-line-function 'c-indent-line)
  (setq-default c-basic-offset flex-indent-level)

  (c-set-offset 'knr-argdecl-intro 0)
  
  ;; remove auto and hungry anything
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(flex\\|\\(l\\(pp\\|l\\|xx\\|\\+\\+\\)\\)\\)$" .
               flex-mode))

(provide 'flex-mode)
;;; flex-mode.el ends here
