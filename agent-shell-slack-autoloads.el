;;; agent-shell-slack-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

;;;### (autoloads nil "agent-shell-slack" "agent-shell-slack.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from agent-shell-slack.el

(autoload 'agent-shell-slack-mode "agent-shell-slack" "\
Forward Slack self-DMs into one Agent Shell buffer.

Enabling this mode prompts for a monitor agent, optional model id, and polling
frequency.  It starts a single temporary Agent Shell that periodically checks
the latest Slack self-DM.  New Slack messages are queued into the Agent Shell
buffer from which the mode was enabled.

If called interactively, enable Agent-Shell-Slack mode if ARG is
positive, and disable it if ARG is zero or negative.  If called from
Lisp, also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'agent-shell-slack "agent-shell-slack" "\
Enable Slack self-DM forwarding in the current Agent Shell buffer." t nil)

(autoload 'agent-shell-slack-version "agent-shell-slack" "\
Show `agent-shell-slack' version." t nil)

(register-definition-prefixes "agent-shell-slack" '("agent-shell-slack--"))

;;;***

(provide 'agent-shell-slack-autoloads)
;;; agent-shell-slack-autoloads.el ends here
