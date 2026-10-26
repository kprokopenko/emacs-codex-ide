;;; codex-ide-transcript-tests.el --- Transcript controller tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Behavior of app-server configuration notifications in parent sessions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

(ert-deftest codex-ide-transcript-child-config-preserves-parent ()
  (dolist (child-fields
           '(((type . "collabAgentToolCall")
              (tool . "spawnAgent")
              (status . "inProgress")
              (receiverThreadIds . ["thread-child"]))
             ((type . "subAgentActivity")
              (kind . "started")
              (agentPath . "/root/child")
              (agentThreadId . "thread-child"))))
    (ert-info ((alist-get 'type child-fields))
      (let ((project-dir (codex-ide-test--make-temp-project))
            (codex-ide-model "gpt-6.1-sol")
            (codex-ide-reasoning-effort "ultra")
            messages)
        (codex-ide-test-with-fixture project-dir
          (codex-ide-test-with-fake-processes
            (let ((session (codex-ide--create-process-session)))
              (setf (codex-ide-session-thread-id session) "thread-parent")
              (with-current-buffer (codex-ide-session-buffer session)
                (codex-ide--insert-input-prompt session "Delegate this")
                (cl-letf (((symbol-function 'codex-ide--request-sync)
                           (lambda (&rest _) nil)))
                  (codex-ide--submit-prompt)))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args) messages))))
                (dolist (method '("item/started" "item/completed"))
                  (ert-info (method)
                    (codex-ide--handle-notification
                     session
                     `((method . ,method)
                       (params
                        . ((threadId . "thread-parent")
                           (item . ((id . "child-activity")
                                    ,@child-fields
                                    (model . "gpt-5.5")
                                    (reasoningEffort . "xhigh")))))))
                    (should-not
                     (codex-ide--session-metadata-get
                      session :submitted-turn-config-mismatch))
                    (should (equal (codex-ide--server-model-name session)
                                   "gpt-6.1-sol"))
                    (should (equal (codex-ide--session-metadata-get
                                    session :reasoning-effort)
                                   "ultra")))))
              (should-not
               (seq-some (lambda (text)
                           (string-match-p "Codex config mismatch:" text))
                         messages)))))))))

(ert-deftest codex-ide-transcript-parent-config-mismatch-is-reported ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        messages)
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-parent")
          (codex-ide--session-metadata-put
           session :submitted-turn-config
           '((model . "gpt-6.1-sol") (effort . "ultra")))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (codex-ide--handle-notification
             session
             '((method . "thread/settings/updated")
               (params
                . ((threadId . "thread-parent")
                   (threadSettings
                    . ((model . "gpt-6.1-sol") (effort . "xhigh"))))))))
          (should (equal (codex-ide--session-metadata-get
                          session :submitted-turn-config-mismatch)
                         '((effort "ultra" "xhigh"))))
          (should (equal (codex-ide--session-metadata-get
                          session :reasoning-effort)
                         "xhigh"))
          (should (seq-some (lambda (text)
                             (string-match-p "Codex config mismatch:" text))
                           messages)))))))

(provide 'codex-ide-transcript-tests)

;;; codex-ide-transcript-tests.el ends here
