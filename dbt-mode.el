;;; dbt-mode.el --- mode for using data build tool in emacs  -*- lexical-binding: t -*-

;;

;; Author: Richard Fulop <mail@richardfulop.com>
;; Keywords: convenience sql
;; Maintainer: Richard Fulop <mail@richardfulop.com>
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: MIT
;; Version: prerelease

;;; Commentary:

;; I haven't tested this on any earlier versions of Emacs, so I'm not providing any warranties for below 28.1.

;;; Code:

(defgroup dbt nil
  "Interact with sql databases using data build tool."
  :prefix "dbt-"
  :group 'sql)

(defun dbt-run-buffer ()
    "Run model from the current buffer."
    (interactive)
    (let ((model (file-name-base buffer-file-name)))
      (async-shell-command (format "dbt run --select %s" model))))

(defun dbt-run ()
  "Run the entire dbt project."
  (interactive)
  (async-shell-command "dbt run"))

(defun dbt-test ()
  "Test the entire dbt project."
  (interactive)
  (async-shell-command "dbt test"))

(defun dbt-format-buffer ()
  "Call dbt-format shell function on the current buffer's file location."
  (interactive)
  (save-buffer)
  (let ((command-text (format "autoload dbt-format; dbt-format --replace -f \"%s\"" (buffer-file-name (window-buffer (minibuffer-selected-window))))))
    (message "Running shell command \"%s\"" command-text)
    (message "Command returned with: %s" (shell-command-to-string command-text)))
  (save-buffer))

(defun dbt-build-buffer ()
    "Run model from the current buffer."
    (interactive)
    (let ((model (file-name-base buffer-file-name)))
      (async-shell-command (format "dbt build --select %s" model))))

(defun dbt-build ()
  "Call dbt build on project in the current directory."
  (interactive)
  (async-shell-command "dbt build"))

(defun dbt-get-compiled-version (file-name)
    "Get the path to the compiled version of the file.
FILE-NAME: the path to the model"
    (let* ((git-root (replace-regexp-in-string "\n" "" (shell-command-to-string "pushd ~/workspace/dbt-analytics; git rev-parse --show-toplevel")))
           (file-name-regex (concat "^" (file-name-nondirectory file-name) "$")))
      (car (directory-files-recursively (concat git-root "/target/compiled/") file-name-regex ))))

(defun dbt-open-compiled ()
  "Open the compiled version of the current buffer file."
  (interactive)
  (find-file (dbt-get-compiled-version buffer-file-name)))

(defun dbt-open-compiled-prd ()
  "Open the compiled version of the current buffer file."
  (interactive)
  (find-file (dbt-get-compiled-version buffer-file-name))
  (perform-replace "dev_richie_" "prd_" nil nil nil)
  (perform-replace "dev_" "prd_" nil nil nil))

(defun dbt-compile ()
  "Call dbt compile on project in the current directory."
  (interactive)
  (async-shell-command "dbt compile"))

(defun dbt-compile-model ()
  "Call dbt compile on model in the current buffer."
  (interactive)
  (let ((model (file-name-base buffer-file-name)))
    (async-shell-command (format "dbt compile --select %s" model))))

;;;###autoload
(define-minor-mode dbt-mode
  "Toggle dbt mode, a local minor mode."
  :global nil
  :group 'dbt
  :lighter " dbt")

;;;###autoload
(add-hook 'sql-mode-hook 'dbt-mode)

(provide 'dbt-mode)

;;; dbt-mode.el ends here
