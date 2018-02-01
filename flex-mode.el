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

(defvar flex-rules-indent-column 25
  "Column to start indentation for code fragments in rules section.")

;;--- Indentation ----------------------------------------------------
;;
;; When inserting '%' and preceded by another '%', attempt to add text
;; properties to the rules section that will be used for special indentation.
;; Text in the region having the 'flex-rules property will be indented
;; by skipping any regex that occurs at the beginning of a line and
;; indenting the following code fragments as C code treating 
;; `flex-rules-indent-column' as if it were the first column.

(defun flex--mark-rules-region ()
  (save-excursion
    (goto-char (point-min))
    (when (ignore-errors (search-forward "%%" nil))
      (let ((start (point)))
        (when (ignore-errors (search-forward "%%" nil))
          (add-text-properties start (point) '(flex-rules t)))))))

(defun flex-insert-% ()
  (interactive)
  (and (eq (char-before) ?%)
       (flex--mark-rules-region))
  (self-insert-command 1))

;; move passed regex
(defun flex-skip-regex ()
  (catch 'done
   (while (not (or (eolp)))
     (skip-syntax-forward "\"")         ;skip strings
     (and (memq (char-after) '(? ?\t))
          (throw 'done t))
     (if (not (eq (char-after) ?\[))
         (forward-char)
       ;; FIXME: ']' can be escaped -- eg. with '^' preceding
       (skip-chars-forward "^]" (line-end-position))))))

;; indent line with rules section (b/w '%%' '%%')
(defun flex-indent-rules-line ()
  (interactive)
  (cond
   ((nth 4 (syntax-ppss))                        ;in comment
    (indent-line-to 2))
   ((and (looking-at-p "\\s-*\\(/\\*\\|\\*/\\)") ;start/end comment
         (looking-back "^\\s-*" (line-beginning-position)))
    (indent-line-to 1))
   ((zerop (current-indentation))                ;regex at bol, indent to rules col
    (save-excursion
      (beginning-of-line)
      (flex-skip-regex)
      (delete-horizontal-space)
      (indent-to-column flex-rules-indent-column)))
   (t
    ;; otherwise, indent using C indent command and add
    ;; additional indent afterward
    (c-indent-line)
    (let ((ci (current-indentation)))
      (when (< ci flex-rules-indent-column)
        (save-excursion
          (back-to-indentation)
          (indent-to (+ ci flex-rules-indent-column))))))))

;; Indent the entire rules section
(defun flex-indent-rules-section ()
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    (when (ignore-errors (search-forward "%%" nil))
      (push-mark (point))
      (when (ignore-errors (search-forward "%%" nil))
        (flex-indent-rules-line-or-region 'region)))))

(defun flex-indent-rules-line-or-region (&optional region)
  (interactive (list (region-active-p)))
  (if (not region)
      (flex-indent-rules-line)
    (let ((beg (region-beginning))
          (end (region-end)))
     (save-excursion
       (goto-char beg)
       (while (< (point) end)
         (flex-indent-rules-line)
         (forward-line))))))

(defun flex-rules-back-to-indentation ()
  (interactive)
  (if (zerop (current-indentation))
      (move-to-column flex-rules-indent-column t)
    (back-to-indentation)))

(defun flex-indent-command ()
  (interactive)
  (if (not (get-text-property (point) 'flex-rules))
      (call-interactively 'c-indent-line-or-region)
    (call-interactively 'flex-indent-rules-line-or-region)
    (flex-rules-back-to-indentation)))

;;--- Commands -------------------------------------------------------

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
(defun flex-compile-and-run ()
  (interactive))

;; Align declarations in region.
(defun flex-align-region-dwim (&optional _arg)
  (interactive))

;;; Navigate
;; FIXME: add markers to regions

;; jump to next section
(defun flex-next-section ()
  (interactive)
  (re-search-forward "%[%}{]" nil 'move))

(defun flex-previous-section ()
  (interactive)
  (re-search-backward "%[%}{]" nil 'move))

;;--- Major Mode -----------------------------------------------------

;; Menu
(defvar flex-menu
  '("Flex"
    ["Edit C source" flex-src-edit t]
    ["Indent entire rules section" flex-indent-rules-section t]
    "---"
    ["Compile" flex-compile t]
    ["Compile and Run" flex-compile-and-run t]))

;; Map
(defvar flex-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil flex-menu)
    (define-key km "%"               #'flex-insert-%)
    (define-key km "{"               #'self-insert-command)
    (define-key km "}"               #'self-insert-command)
    (define-key km (kbd "C-c '")     #'flex-src-edit)
    (define-key km (kbd "M-s-n")     #'flex-next-section)
    (define-key km (kbd "M-s-p")     #'flex-previous-section)
    (define-key km (kbd "<f5>")      #'flex-compile)
    (define-key km (kbd "<backtab>") #'flex-indent-rules-line-or-region)
    (define-key km (kbd "M-M")       #'flex-rules-back-to-indentation)
    (define-key km (kbd "TAB")       #'flex-indent-command)
    km))

;;;###autoload
(define-derived-mode flex-mode c++-mode "Flex"
  "Major mode for editing flex files derived from `c-mode'.\n
\\{flex-mode-map}"
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  ;; Indentation
  (make-local-variable 'c-basic-offset)
  (make-local-variable 'c-offsets-alist)
  (setq-local indent-line-function 'c-indent-line)
  (setq-default c-basic-offset flex-indent-level)

  (c-set-offset 'knr-argdecl-intro 0)
  
  ;; remove auto and hungry anything
  (c-toggle-electric-state -1)
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)

  ;; mark the rules region in buffer
  (flex---mark-rules-region))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(f?lex\\|\\(l\\(pp\\|l\\|xx\\|\\+\\+\\)\\)\\)$" .
               flex-mode))

(provide 'flex-mode)
;;; flex-mode.el ends here
