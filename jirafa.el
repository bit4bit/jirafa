;;; giraorg -- minimal gira ui

(require 'jirafalib)
(defvar jirafa-entry-format "[%s] %s")


(defun jirafa-insert-from-issue--at-point (issue-id)
  "insert org entry in current position."
  (interactive "sJIRA ISSUE: ")
  (let* ((issue (jirafalib-issue--get-by-id issue-id))
         (summary (jirafalib-issue--get-field issue 'summary))
         (title (format jirafa-entry-format issue-id summary)))
      (org-insert-todo-heading-respect-content)
      (insert title)
  ))


(provide 'jirafa)
