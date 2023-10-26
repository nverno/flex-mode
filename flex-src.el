;;; flex-src --- edit src code in pop-up buffer -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/flex-mode
;; Package-Requires: 
;; Created: 19 November 2016

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
;;; Code:
(require 'flex-mode)

;; major mode for src code editing buffer
(defvar flex-src-major-mode 'c++-mode)

;; -------------------------------------------------------------------

;; keep track of original source buffer
(defvar flex-src--prev-buffer)

;; create buffer for editing source code
(defun flex-src--buffer (mode &optional code)
  (let ((buff (get-buffer-create
               (concat "*flex-src [" (symbol-name mode) "]*")))
        (prev (current-buffer)))
    (with-current-buffer buff
      (kill-all-local-variables)
      (funcall mode)
      (flex-src-mode)
      (setq-local flex-src--prev-buffer prev)
      (when code
        (insert code))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun flex-src-edit (&optional start end)
  "Edit source code in another buffer, inserting at point when 
finished."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))))
  (let ((code (if (and start end)
                  (buffer-substring-no-properties start end))))
    (when code
      (delete-region start end))
    (flex-src--buffer flex-src-major-mode code)))

;;——— Minor Mode —————————————————————————————————————————————————————

(defun flex-src-exit ()
  "Close editing buffer, inserting code into source buffer."
  (interactive)
  (let ((code (buffer-string))
        (prev (bound-and-true-p flex-src--prev-buffer)))
    (kill-buffer (current-buffer))
    (with-current-buffer prev
      (flex-src-insert code)
      (pop-to-buffer (current-buffer)))))

;; insert code, indenting to align column
(defun flex-src-insert (code)
  (let ((start (point))
        (_ (insert code))
        (end (line-number-at-pos)))
    (goto-char start)
    (while (not (or (eobp)
                    (> (line-number-at-pos) end)))
      (indent-to flex-rules-continuation-offset)
      (forward-line 1))))

(defvar flex-src-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-x C-s") 'flex-src-exit)
    km))

(define-minor-mode flex-src-mode
  "Flex source editing minor mode."
  :lighter " FlexSrc")

(provide 'flex-src)
;;; flex-src.el ends here
