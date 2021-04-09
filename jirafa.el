;;; giraorg -- minimal gira ui

(require 'jirafalib)

(defvar-local jirafa-entry-format "[%s] %s")
(defvar-local jirafa-cache-project-collection nil)
(defvar-local jirafa-cache-project-assignee-collection (make-hash-table))
(defvar-local jirafa-cache-issuetype-collection (make-hash-table))
(defvar jirafa-jira-id-insert-format "jira: %s")
(defvar jirafa-jira-id-insert-bottom-callback 'jirafa-jira-id-insert-bottom-end-buffer)

(defun jirafa-refresh-cache ()
  "Refresh cache."
  (interactive)
  (jirafa-cache-project-collection-load)
  (jirafa-cache-issuetype-collection-load)
  (jirafa-cache-project-assignee-collection-load))


(defun jirafa-insert-from-issue--at-point (issue-id)
  "insert org entry in current position."
  (interactive "sJIRA ISSUE: ")
  (let* ((issue (jirafalib-issue--get-by-id issue-id))
         (summary (jirafalib-issue--get-field issue 'summary))
         (title (format jirafa-entry-format issue-id summary)))
      (org-insert-todo-heading-respect-content)
      (insert title)
      (org-entry-put nil "JIRAFA-JIRA-ID" issue-id)
  ))

(defun jirafa-open-url--at-point ()
  "open browser for current jira issue"
  (interactive)
  (let ((issue (org-entry-get nil "JIRAFA-JIRA-ID")))
    (if issue
        (browse-url (jirafalib-issue--url issue))
      (message "not found JIRA-ID"))))

(defun jirafa-insert-new-issue--at-point ()
  "Insert a new issue."
  (interactive)
  (let* ((project-name (completing-read "PROJECT: " jirafa-cache-project-collection))
         (project-id (jirafa-project-id-from-project-collection project-name))
         (assignee-name (completing-read "ASSIGNEE: " (jirafa-assignee-collection project-id)))

         (issue-type (completing-read "ISSUETYPE: " (jirafa-project-issuetypes-collection project-id)))
         (issuetype-id (jirafa-issuetype-id-from-issuetypes-collection project-id issue-type))
         (assignee-id (jirafa-assignee-id-from-collection project-id assignee-name))
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

(defun jirafa-jira-id-insert-from-current-clock--bottom ()
  "Insert jira id from org with current clock active"
  (interactive)
  (goto-char (point-max))
  (let ((text
         (format jirafa-jira-id-insert-format (jirafa-jira-from-current-clock))))
    (apply jirafa-jira-id-insert-bottom-callback (list text))))

(defun jirafa-jira-id-insert-bottom-end-buffer (text)
  "insert to end of buffer"
  (goto-char (point-max))
  (insert text))

(defun jirafa-jirafalib-project-issuetypes-collection (id-or-key)
  (let* ((project (jirafalib-project-detail--get-by-id-or-key id-or-key))
         (issuetypes (cdr (assoc 'issueTypes project))))
    (seq-map (lambda (issuetype)
               (cons
                (cdr (assoc 'name issuetype))
                (cdr (assoc 'id issuetype))))
             issuetypes)
    ))

(defun jirafa-project-issuetypes-collection (project-id)
  (gethash project-id jirafa-cache-issuetype-collection))

(defun jirafa-issuetype-id-from-issuetypes-collection (project-id issue-name)
  (let ((collection (jirafa-project-issuetypes-collection project-id)))
    (cdr (assoc issue-name collection))
  ))

(defun jirafa-assignee-collection (project-id)
  (gethash project-id jirafa-cache-project-assignee-collection))

(defun jirafa-jirafalib-assignee-collection (project-id)
  (let* ((users (jirafalib-project-users-assignable--get-by-id-or-key project-id)))
    (seq-map (lambda (user)
               (cons
                (cdr (assoc 'displayName user))
                (cdr (assoc 'accountId user))))
             users)))

(defun jirafa-assignee-id-from-collection (project-id assignee-name)
  (let ((collection (jirafa-assignee-collection project-id)))
    (cdr (assoc assignee-name collection))))

(defun jirafa-project-id-from-project-collection (project-name)
  (cdr (assoc project-name jirafa-cache-project-collection)))

(defun jirafa-jirafalib-project-collection ()
  (seq-map (lambda (project)
             (cons
              (cdr (assoc 'name project))
              (cdr (assoc 'id project))))
           (jirafalib-project--list)))

(defun jirafa-cache-project-collection-load ()
  (let ((collection (jirafa-jirafalib-project-collection)))
    (setq jirafa-cache-project-collection collection)))

(defun jirafa-cache-issuetype-collection-load ()
  (dolist (project jirafa-cache-project-collection)
    (let* ((project-id (cdr project))
          (issuetype-collection (jirafa-jirafalib-project-issuetypes-collection project-id)))
      (clrhash jirafa-cache-project-assignee-collection)
      (puthash project-id issuetype-collection jirafa-cache-issuetype-collection)
      )))

(defun jirafa-cache-project-assignee-collection-load ()
  (dolist (project jirafa-cache-project-collection)
    (let* ((project-id (cdr project))
           (assignee-collection (jirafa-jirafalib-assignee-collection project-id)))
      (clrhash jirafa-cache-project-assignee-collection)
      (puthash project-id assignee-collection jirafa-cache-project-assignee-collection)
      )))


(defun jirafa-jira-from-current-clock ()
  "Get jira id from current clock"
  (with-current-buffer (org-clock-is-active)
    (save-excursion
      (save-restriction
        (org-back-to-heading t)
        (let* ((element (cadr (org-element-at-point)))
               (jira-id (org-entry-get nil "JIRAFA-JIRA-ID" t)))
          jira-id
          )))))


(provide 'jirafa)
