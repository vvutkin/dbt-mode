(defun dbt-run ()
  (interactive)
  (async-shell-command "dbt run --fail-fast"))

(defun dbt-test ()
  (interactive)
  (async-shell-command "dbt test --fail-fast"))

(defvar dbt-mode-hook nil)
(defvar dbt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c C-b C-r" 'dbt-run)
    (define-key map "C-c C-b C-t" 'dbt-test)
    map)
  "Keymap for DBT major mode")
(defvar dbt-mode ()
  "Minor mode for dbt sql"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dbt-mode-map))

(provide 'dbt-mode)
