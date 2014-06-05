;;; smart-shift.el --- Smart shift text left/right.

;; Copyright © 2014 Bin Huang <huangbin88@foxmail.com>

;; Author: Bin Huang <huangbin88@foxmail.com>
;; Maintainer: Bin Huang <huangbin88@foxmail.com>
;; URL: https://github.com/hbin/smart-shift
;; Created: 5th Jun 2014
;; Version: 0.1
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; `smart-shift' make it easy to shift current line or region to
;; left/right according to current major mode indentation.

;;; Code:

(defvar smart-shift-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c [") 'smart-shift-left)
    (define-key map (kbd "C-c ]") 'smart-shift-right)
    map)
  "Keymap for `smart-shift-mode'.")

(defvar smart-shift-indentation-level
  (lambda ()
    (cond
     ((eq major-mode 'ruby-mode) ruby-indent-level)
     ((eq major-mode 'js-mode) js-indent-level)
     ((eq major-mode 'coffee-mode) coffee-tab-width)
     ((eq major-mode 'css-mode) css-indent-offset)
     ((eq major-mode 'scss-mode) css-indent-offset)
     ((eq major-mode 'yaml-mode) yaml-indent-offset)
     ((eq major-mode 'c-mode) c-basic-offset)
     ((eq major-mode 'sh-mode) sh-basic-offset)
     ((eq major-mode 'slim-mode) slim-indent-offset)
     ((eq major-mode 'python-mode) python-indent-offset)
     ((eq major-mode 'html-mode) sgml-basic-offset)
     ((eq major-mode 'web-mode)
      (message web-mode-content-type)
      (cond ((string= web-mode-content-type "css")
             web-mode-css-indent-offset)
            ((member web-mode-content-type '("javascript" "json" "jsx" "php"))
             web-mode-code-indent-offset)
            (t web-mode-markup-indent-offset))) ; xml, html, etc...
     ((eq major-mode 'swift-mode) swift-indent-offset)
     (t tab-width)))

  "Indentation level according to current major mode.

If none of modes listed below match, use the `tab-width' as default.
Can also be set explictly to a number or a function called without arguments
and evaluting to a number.")

(defun smart-shift-right (&optional arg)
  "Shift the line or region to the ARG times to the right."
  (interactive "P")
  (let ((deactivate-mark nil)
        (beg (if (use-region-p)
                 (save-excursion
                   (goto-char (region-beginning))
                   (line-beginning-position))
               (line-beginning-position)))
        (end (if (use-region-p)
                 (save-excursion
                   (goto-char (region-end))
                   (line-end-position))
               (line-end-position)))
        (times (cond ((equal arg nil) 1) ; universal-argument not called
                     ((equal arg '(4)) 4) ; C-u
                     (t arg)))            ; all other cases
        (indentation-level (if (functionp smart-shift-indentation-level)
                               (funcall smart-shift-indentation-level)
                             smart-shift-indentation-level)))
    (indent-rigidly beg end (* times indentation-level))))

(defun smart-shift-left (&optional arg)
  "Shift the line or region to the ARG times to the left."
  (interactive "P")
  (let ((times (cond ((equal arg nil) 1) ; universal-argument not called
                     ((equal arg '(4)) 4) ; C-u
                     (t arg))))           ; all other cases
    (smart-shift-right (* -1 times))))

;;;###autoload
(define-minor-mode smart-shift-mode
  "Shift line/region to left/right."
  :init-value nil
  :lighter ""
  :keymap smart-shift-mode-map)

(provide 'smart-shift)

;;; smart-shift.el ends here
