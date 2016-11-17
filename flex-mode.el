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

(defgroup flex nil
  "Major mode for flex files."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages
  :prefix "flex-")

(defcustom flex-indent-level 4
  "Amount by which expressions are indented."
  :type 'integer
  :group 'flex)

;; ------------------------------------------------------------

(defvar flex-decls-opener "%{")
(defvar flex-decls-closer "%}")
(defvar flex-grammar-delim "%%")

;; Font-locking

;; Indentation

(defalias 'flex-indent-command #'c-indent-command)

;; ------------------------------------------------------------
;; User Functions

(defun flex-compile ()
  "Compile"
  (interactive))

(defun flex-compile-run ()
  "Compile and run on test file."
  (interactive))

(defun flex-align-region-dwim (&optional _arg)
  "Align declarations in region."
  (interactive))

;; ------------------------------------------------------------
;;* Major Mode

;; Menu
(defvar flex-menu
  '("Flex"
    ["Compile" flex-compile]
    ["Align Declarations" flex-align-decls]
    ["Compile and Run" flex-compile-and-run]))

;; Map
(defvar flex-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil flex-menu)
    (define-key map "{"         #'self-insert-command)
    (define-key map "}"         #'self-insert-command)
    (define-key map (kbd "TAB") #'flex-indent-command)
    map)
  "Flex mode map.")

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
