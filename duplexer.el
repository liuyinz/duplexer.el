;;; duplexer.el --- Handle conflicts between local minor modes and reuse rules -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2023, 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords: tools
;; Homepage: https://github.com/liuyinz/duplexer.el

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides method to handle conflicts between local minor modes
;; and reuse rules.  When a CALLER mode enable, call related CALLEE modes with
;; arguments and restore them automatically after the CALLER mode disabled.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'find-func)

(defgroup duplexer nil
  "Handle conflicting local minor modes."
  :group 'duplexer)

(defcustom duplexer-alist nil
  "Alist of elements (CALLER . RULES).
CALLER is a local minor mode.
RULES is a list contains element like (CALLEE ARG).
When CALLER is enabled, call CALLEE with ARG.  CALLEE usually
is a local minor mode, ARG is 1 or -1."
  :type '(alist
          :key-type symbol
          :value-type (alist
                       :key-type function
                       :value-type (group integer)))
  :group 'duplexer)

(defcustom duplexer-groups nil
  "Alist of elements (GROUP . RULES), which stores rules for a related topic.
GROUP is a symbol name, RULES is a list of elements (CALLEE ARG)."
  :type '(alist
          :key-type symbol
          :value-type (alist
                       :key-type function
                       :value-type (group integer sexp)))
  :group 'duplexer)

(defcustom duplexer-fallback-alist
  '((read-only-mode . buffer-read-only)
    (save-place-local-mode . save-place-mode))
  "Alist of element (MINOR-MODE . MIOR-MODE-VARIABLE).
Some minor modes use another variable instead of MODE to store the state of the
mode.  Add these cases here as fallback."
  :type '(alist :key-type (symbol :tag "Minor mode name")
                :value-type (symbol :tag "Minor mode variable"))
  :group 'duplexer)

(defcustom duplexer-force nil
  "If non-nil, apply rule by force even conflicts with rules in restore alist."
  :type 'boolean
  :group 'duplexer)

(defcustom duplexer-quiet nil
  "If non-nil, do not print warnings in echo area."
  :type 'boolean
  :group 'duplexer)

(defvar-local duplexer-restore-alist nil
  "Alist of elements (CALLER . RULES), which to store rules to restore .")

(defvar duplexer-log-buffer "*duplexer-log*"
  "Name of the duplexer's log buffer.")

(defun duplexer--message (caller rule warning)
  "Print message of current duplexer in format of CALLER RULE WARNING."
  (let ((str (format "[Duplexer] BUFFER: %S, CALLER: %S, RULE: %S, WARNING: %s"
                     (current-buffer) caller rule warning)))
    (with-current-buffer (get-buffer-create duplexer-log-buffer)
      (goto-char (point-max))
      (insert "\n" str))
    (unless duplexer-quiet (message str))))

(defun duplexer--minor-mode-p (mode)
  "Return t if MODE is a minor mode."
  (and (functionp mode)
       (progn
         (when (autoloadp (symbol-function mode))
           (load (cdr (find-function-library mode)) nil t))
         (or (memq mode minor-mode-list)
             (memq (alist-get mode duplexer-fallback-alist) minor-mode-list)))))

(defun duplexer--local-minor-mode-p (mode)
  "Return t if MODE is a local minor mode."
  (and (duplexer--minor-mode-p mode)
       (or (local-variable-if-set-p mode)
           (local-variable-if-set-p (alist-get mode duplexer-fallback-alist)))))

(defun duplexer--callers (&optional restore)
  "Return list of callers of `duplexer-alist'.
If optional arg RESTORE is non-nil, apply on `duplexer-restore-alist'"
  (seq-uniq (mapcar #'car (if restore duplexer-restore-alist duplexer-alist))))

(defun duplexer--rules (&optional caller restore)
  "Return list of rules of `duplexer-alist'.
If optional arg CALLER is non-nil, only return rules related to it.
If optional arg RESTORE is non-nil, apply on `duplexer-restore-alist'"
  (seq-uniq (seq-reduce #'append
                        (mapcar
                         (if caller (lambda (x) (and (eq caller (car x)) (cdr x)))
                           #'cdr)
                         (if restore duplexer-restore-alist duplexer-alist)) nil)))

(defun duplexer--normalize (&optional mode)
  "Return alist which consists of uniq and legal rules related to MODE.
MODE is optional argument, if it's nil, return rules of all callers."
  (cl-loop for caller in (or (and mode (list mode)) (duplexer--callers))
           when (cl-loop for rule in (duplexer--rules caller)
                         if (or (alist-get rule duplexer-groups)
                                (and (consp rule) (list rule)))
                         append it into extracted
                         else do (duplexer--message
                                  caller rule
                                  "rule cannot found in duplexer groups")
                         finally return (cl-remove-duplicates
                                         extracted
                                         :key #'car :test #'eq :from-end t))
           collect (cons caller it) into result
           finally return (reverse result)))

(defun duplexer--apply (caller &optional restore)
  "Apply rules of CALLER in current buffer.
If RESTORE is non-nil, restore callee modes if exists."
  (let ((result))
    (dolist (rule (alist-get caller (if restore
                                        duplexer-restore-alist
                                      (duplexer--normalize caller))))
      (seq-let (callee arg) rule
        (cond
         ((not (fboundp callee))
          (duplexer--message caller rule (format "%s not found" callee)))
         ((not (duplexer--minor-mode-p callee))
          (duplexer--message caller rule (format "%s isn't a minor mode" callee)))
         ((xor (memq callee (cl-union local-minor-modes global-minor-modes))
               (equal 1 arg))
          (if restore
              (funcall callee arg)
            ;; avoid conflict rule if setting
            (let ((conflict (member (list callee (- arg))
                                    (duplexer--rules nil t))))
              (unless (and conflict (null duplexer-force))
                ;; same rule in different callers would only be pushed at first time
                (funcall callee arg)
                (push (list callee (- arg)) result)
                (when (not (duplexer--local-minor-mode-p callee))
                  (duplexer--message caller rule
                                     (format "%s isn't local minor mode" callee))))
              (and conflict
                   (duplexer--message caller rule
                                      (format "rule conflicts and applied by force: %S"
                                              duplexer-force)))))))))
    (or restore result)))

(defun duplexer--toggle (caller)
  "Cancel or restore other callee modes when toggle CALLER."
  (if (memq caller local-minor-modes)
      (when-let ((rules (duplexer--apply caller)))
        (push (cons caller rules) duplexer-restore-alist))
    (duplexer--apply caller 'restore)
    (setq duplexer-restore-alist (assq-delete-all caller duplexer-restore-alist))))

;;;###autoload
(defun duplexer-add (caller rules)
  "Add new (CALLER . RULES) to `duplexer-alist'."
  (when (delq nil rules)
    (let ((orig (copy-tree duplexer-alist)))
      (if-let ((record (assq caller duplexer-alist)))
          (dolist (rule rules)
            (when rule
              (if-let ((item (assq (car rule) (cdr record))))
                  (setcdr item (cdr rule))
                (push rule (cdr record)))))
        (push (cons caller rules) duplexer-alist))
      ;; readd hook if duplexer-alist updates
      (and (bound-and-true-p duplexer-mode)
           (not (equal duplexer-alist orig))
           (duplexer-mode 1)))))

;;;###autoload
(define-minor-mode duplexer-mode
  "Minor mode to handle conflicts between minor modes."
  :group 'duplexer
  (let ((cmd (if duplexer-mode #'add-hook #'remove-hook)))
    (dolist (caller (duplexer--callers))
      (funcall cmd (intern (concat (symbol-name caller) "-hook"))
               (apply-partially #'duplexer--toggle caller)))))

(provide 'duplexer)
;;; duplexer.el ends here
