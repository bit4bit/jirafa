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

(defun jirafa-insert-new-issue--at-point ()
  "Insert a new issue."
  (interactive)
  (let* ((project-name (completing-read "PROJECT: " (jirafa-jirafalib-project-collection)))
         (project-id (project-id-from-project-collection project-name))
         (assignee-name (completing-read "ASSIGNEE: " (jirafa-jirafalib-assignee-collection project-id)))

         (issue-type (completing-read "ISSUETYPE: " (jirafa-jirafalib-project-issuetypes-collection
                                                     project-id)))
         (issuetype-id (issuetype-id-from-issuetypes-collection project-id issue-type))
         (assignee-id (assignee-id-from-collection project-id assignee-name))
         (summary (read-string "SUMMARY: " ""))
         (description (read-string "DESCRIPTION: " "")))
    ;; retorna
    ; ((id . "10001") (key . "TEL-2") (self . "https://xxx.atlassian.net/rest/api/2/issue/10001"))
    (let* ((response (jirafalib-issue--insert project-id issuetype-id assignee-id summary description))
           (key (cdr (assoc 'key response))) 
           (id (cdr (assoc 'id response)))
           (title (format jirafa-entry-format key summary)))
      (org-insert-todo-heading-respect-content)
      (insert title)
      (org-entry-put nil "JIRAFA-JIRA-ID" id)
      (org-entry-put nil "JIRAFA-JIRA-KEY" key))
    ))

(defun jirafa-jirafalib-project-collection ()
  (seq-map (lambda (project)
             (cons
              (cdr (assoc 'name project))
              (cdr (assoc 'id project))))
           (jirafalib-project--list)))

(defun jirafa-jirafalib-project-issuetypes-collection (id-or-key)
  (let* ((project (jirafalib-project-detail--get-by-id-or-key id-or-key))
         (issuetypes (cdr (assoc 'issueTypes project))))
    (seq-map (lambda (issuetype)
               (cons
                (cdr (assoc 'name issuetype))
                (cdr (assoc 'id issuetype))))
             issuetypes)
    ))

(defun jirafa-jirafalib-assignee-collection (project-id)
  (let* ((users (jirafalib-project-users-assignable--get-by-id-or-key project-id)))
    (seq-map (lambda (user)
               (cons
                (cdr (assoc 'displayName user))
                (cdr (assoc 'accountId user))))
             users)))

               
(defun project-id-from-project-collection (project-name)
  (let ((collection (jirafa-jirafalib-project-collection)))
    (cdr (assoc project-name collection))
    ))

(defun issuetype-id-from-issuetypes-collection (project-id issue-name)
  (let ((collection (jirafa-jirafalib-project-issuetypes-collection project-id)))
    (cdr (assoc issue-name collection))
  ))

(defun assignee-id-from-collection (project-id assignee-name)
  (let ((collection (jirafa-jirafalib-assignee-collection project-id)))
    (cdr (assoc assignee-name collection))))

(provide 'jirafa)

