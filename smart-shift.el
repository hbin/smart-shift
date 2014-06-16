;;; smart-shift.el --- Smart shift text left/right.

;; Copyright © 2014 Bin Huang <huangbin88@foxmail.com>

;; Author: Bin Huang <huangbin88@foxmail.com>
;; Maintainer: Bin Huang <huangbin88@foxmail.com>
;; URL: https://github.com/hbin/smart-shift
;; Created: 5th Jun 2014
;; Version: 0.2
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

(defgroup smart-shift nil
  "Shift line/region by inferred indentation level."
  :prefix "smart-shift-"
  :group 'convenience)

;;;###autoload
(defcustom smart-shift-mode-alist
  '((lisp-mode . lisp-body-indent)
    (emacs-lisp-mode . lisp-body-indent)

    ;; Modes directly supported by CC Mode
    (c-mode . c-basic-offset)
    (c++-mode . c-basic-offset)
    (objc-mode . c-basic-offset)
    (java-mode . c-basic-offset)
    (idl-mode . c-basic-offset)
    (pike-mode . c-basic-offset)
    (awk-mode . c-basic-offset)

    (ruby-mode . ruby-indent-level)
    (python-mode . python-indent-offset)
    (swift-mode . swift-indent-offset)

    (js-mode . js-indent-level)
    (js2-mode . js2-basic-offset)
    (coffee-mode . coffee-tab-width)

    (css-mode . css-indent-offset)
    (slim-mode . slim-indent-offset)
    (html-mode . sgml-basic-offset)
    (web-mode . (lambda ()
                  (cond ((string= web-mode-content-type "css")
                         web-mode-css-indent-offset)
                        ((member web-mode-content-type '("javascript" "json" "jsx" "php"))
                         web-mode-code-indent-offset)
                        (t web-mode-markup-indent-offset)))) ; xml, html, etc...

    (sh-mode . sh-basic-offset)
    (yaml-mode . yaml-indent-offset)
    (text-mode . tab-width)
    (fundamental-mode . tab-width))
  "Alist which maps major modes to its indentation-level."
  :type '(repeat (cons (symbol :tag "Major mode name")
                       (choice (function :tag "Method evaluting to a number")
                               (integer :tag "Indentation level"
                                        :value tab-width))))
  :group 'smart-shift)

;;;###autoload
(defvar smart-shift-indentation-level nil
  "Variable used to specify the indentation-level for the current buffer.")
(make-variable-buffer-local 'smart-shift-indentation-level)

;;;###autoload
(defun smart-shift-infer-indentation-level ()
  "Infer indentation-level of current major mode."
  (let ((offset (assoc-default major-mode smart-shift-mode-alist
                               (lambda (k v)
                                 (derived-mode-p k)))))
    (cond ((numberp offset) offset)
          ((functionp offset) (funcall offset))
          ((symbolp offset) (symbol-value offset))
          (t tab-width))))

;;;###autoload
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
                   (if (bolp)
                       (1- (region-end))
                     (line-end-position)))
               (line-end-position)))
        (times (cond ((equal arg nil) 1) ; universal-argument not called
                     ((equal arg '(4)) 4) ; C-u
                     (t arg)))            ; all other cases
        (shift (or smart-shift-indentation-level
                   (smart-shift-infer-indentation-level)
                   tab-width)))
    (indent-rigidly beg end (* times shift))
    (smart-shift-override-local-map)))

;;;###autoload
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
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c [") 'smart-shift-left)
            (define-key map (kbd "C-c ]") 'smart-shift-right)
            map))

(defun smart-shift-override-local-map ()
  "Override local key map for continuous indentation."
  (setq overriding-local-map
        (let ((map (copy-keymap smart-shift-mode-map)))
          (define-key map (kbd "]") 'smart-shift-right)
          (define-key map (kbd "[") 'smart-shift-left)
          (define-key map [t] 'smart-shift-pass-through) ;done with shifting
          map)))

;;;###autoload
(defun smart-shift-pass-through ()
  "Finish shifting and invoke the corresponding command."
  (interactive)
  (setq overriding-local-map nil)
  (let* ((keys (progn
                 (setq unread-command-events
                       (append (this-single-command-raw-keys)
                               unread-command-events))
                 (read-key-sequence-vector "")))
         (command (and keys (key-binding keys))))
    (when (commandp command)
      (call-interactively command))))

;;;###autoload
(defun smart-shift-mode-on ()
  "Turn on smart-shift mode."
  (smart-shift-mode 1))

;;;###autoload
(defun smart-shift-mode-off ()
  "Turn off smart-shift mode."
  (smart-shift-mode 0))

;;;###autoload
(define-globalized-minor-mode global-smart-shift-mode
  smart-shift-mode smart-shift-mode-on)

(provide 'smart-shift)

;;; smart-shift.el ends here
