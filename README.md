# jirafa

!!NOT USE THIS, org-mode it's all you need.

*but the reality is impure*

## add api key to ~/.authinfo

example
~~~
machine xxxx.atlassian.net login fuck@company.com password XXXXXXX
~~~


## example installation .emacs

~~~
(require 'jirafa)

(setq jirafalib-host "xxxx.atlassian.net"
~~~

##  example usage

inside emacs in a buffer with org mode enable run **jirafa-insert-from-issue--at-point**

### commands

  ** **jirafa-refresh-cache** load cache
  ** **jirafa-insert-new-issue--at-point** insert a issue on jira and entry todo
  ** **jirafa-insert-from-issue--at-point** insert a entry todo from remote issue
  ** **jirafa-open-url--at-point** use browse-url for open current issue in org entry, link with [Key-Bindings](https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html)
