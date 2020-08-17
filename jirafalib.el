;;; giraorg -- giralib a client for gira api

(defvar jirafalib-host  "gira.atlassian.com")

(defun jirafalib-get-credentials ()
  ""
  (let ((found (nth 0 (auth-source-search :host jirafalib-host))))
    (list
     (plist-get found :user)
     (funcall (plist-get found :secret)))))

(defun jirafalib-credentials-to-basic-auth-string (user pass)
  "build authorization basic"
  (let ((text (concat user ":" pass)))
    (base64-encode-string text)))


(defun jirafalib-issue--get-by-id (id)
  "query a issue by ID"
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          (list '("Content-Type" . "application/json")
                (cons "Authorization" (concat "Basic" " "
                                              (apply 'jirafalib-credentials-to-basic-auth-string
                                                     (jirafalib-get-credentials)))))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "https://" jirafalib-host "/rest/api/2/issue/" id))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (json-read))))

(defun jirafalib-issue--get-field (issue field)
  "get summary from ISSUE"
  (cdr (assoc field (assoc 'fields issue))))



       
(provide 'jirafalib)
