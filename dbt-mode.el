;;; dbt-mode.el --- mode for using data build tool in emacs  -*- lexical-binding: t -*-

;;

;; Author: Richard Fulop <mail@richardfulop.com>
;; Keywords: convenience sql
;; Maintainer: Richard Fulop <mail@richardfulop.com>

;;; Commentary:

;;

;;; Code:

(defgroup dbt nil
  "Interact with sql databases using data build tool"
  :prefix "dbt-"
  :group 'sql)

(defcustom dbt-project-root nil
  "Set the root path for the dbt project"
  :group 'dbt)

(defun dbt-run-buffer ()
    "Run model. If specified, run `model-name`
     otherwise, run the model in the current buffer"
    (interactive)
    (let ((model (file-name-base buffer-file-name))
          (proj-directory (file-name-directory buffer-file-name)))
      (async-shell-command (format "dbt run --select %s" model))))

(defun dbt-run ()
  "Run the entire dbt project"
  (interactive)
  (async-shell-command "dbt run"))

(defun dbt-test ()
  "Test the entire dbt project"
  (interactive)
  (async-shell-command "dbt test"))

(defun dbt-format-buffer ()
  (interactive)
  (save-buffer)
  (set 'command-text (format "autoload dbt-format; dbt-format --replace -f \"%s\"" (buffer-file-name (window-buffer (minibuffer-selected-window)))))
  (message "Running shell command \"%s\"" command-text)
  (message "Command returned with: %s" (shell-command-to-string command-text))
  (save-buffer))

(define-minor-mode dbt-mode
  "Toggle dbt mode, a global minor mode"
  :global nil
  :group 'dbt
  :interactive `(sql-mode)
  :lighter " dbt"
  :keymap '(([]))
  (unless dbt-project-root (setq dbt-project-root (file-name-directory buffer-file-name))))

(provide 'dbt)

;;; dbt-mode.el ends here
