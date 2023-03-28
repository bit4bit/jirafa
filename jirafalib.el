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


(defun jirafalib-issue--insert (project-id issuetype-id assignee summary description)
  "Insert a new issue"
  (json-api--POST "/rest/api/2/issue/"
                  `(
                    ("fields"
                     ("assignee" ("accountId" ,@assignee))
                     ("project" ("id" ,@project-id))
                     ("issuetype" ("id" ,@issuetype-id))
                     ("summary" ,@summary)
                     ("description" ,@description)))
                  )
  )

(defun jirafalib-issue--get-by-id (id)
  "query a issue by ID"
  (json-api--GET (concat "/rest/api/2/issue/" id)))

(defun jirafalib-project--list ()
    "Query projects."
    (seq-map (lambda (project)
               (list
                (assoc 'id project)
                (assoc 'key project)
                (assoc 'name project)
                (assoc 'self project)
                )
               )
             (json-api--GET "/rest/api/2/project")))

(defun jirafalib-project-users-assignable--get-by-id-or-key (id)
  "Query al user assignable to project."
  (let ((users (json-api--GET (concat "/rest/api/2/user/assignable/search?project=" id))))
    (seq-map (lambda (user)
               (list
                (assoc 'key user)
                (assoc 'accountId user)
                (assoc 'displayName user)))
                users)))
  

(defun jirafalib-project-detail--get-by-id-or-key (id)
  "Query project detail by ID."
  (let ((project (json-api--GET (concat "/rest/api/2/project/" id))))
    (list
     (assoc 'id project)
     (assoc 'key project)
     (assoc 'name project)
     (assoc 'self project)
     (assoc 'issueTypes project))
    ))



(defun http--POST (endpoint body)
  "http POST"
  (let* ((url-request-method "POST")
         (url-request-data body)
         (url-request-extra-headers
          (list '("Content-Type" . "application/json")
                '("Accept-Charset" . "utf-8")
                (cons "Authorization" (concat "Basic" " "
                                              (apply 'jirafalib-credentials-to-basic-auth-string
                                                     (jirafalib-get-credentials)))))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "https://" jirafalib-host endpoint))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max))
     ))
  )

(defun http--GET (endpoint)
  "Query a raw http GET for ENDPOINT."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          (list '("Content-Type" . "application/json")
                '("Accept-Charset" . "utf-8")
                (cons "Authorization" (concat "Basic" " "
                                              (apply 'jirafalib-credentials-to-basic-auth-string
                                                     (jirafalib-get-credentials)))))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "https://" jirafalib-host endpoint))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (let ((text (buffer-substring (point) (point-max))))
        (decode-coding-string text 'utf-8))
     ))
  )

(defun json-api--GET (endpoint)
  "Query a GET and return LIST"
  (json-read-from-string (http--GET endpoint)))

(defun json-api--POST (endpoint body)
  "Post request"
  (json-read-from-string (http--POST endpoint
                                     (json-encode-list body))))

(defun jirafalib-issue--get-field (issue field)
  "get summary from ISSUE"
  (cdr (assoc field (assoc 'fields issue))))


(defun jirafalib-issue--url (issue)
  "issue url"
  (concat "https://" jirafalib-host "/browse/" issue))

       
(provide 'jirafalib)
