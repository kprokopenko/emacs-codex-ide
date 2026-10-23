;;; codex-ide-global-status-tests.el --- Global status tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-global-status)

(defmacro codex-ide-test-with-global-status (&rest body)
  "Run BODY with isolated global indicators and session state."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (codex-ide-test-with-fixture temporary-file-directory
				  (codex-ide-test-with-fake-processes
				   (let ((global-mode-string '("clock"))
					 (codex-ide-session-event-hook nil)
					 (codex-ide-global-status-mode nil)
					 (codex-ide-global-status--origin nil)
					 (codex-ide-global-status--target nil))
				     (unwind-protect (progn ,@body)
				       (codex-ide-global-status-mode -1)))))))

(cl-defun codex-ide-test--global-status-session (name status &key query-only dead)
  "Create a tracked chat named NAME with STATUS, QUERY-ONLY and DEAD options."
  (let ((session (make-codex-ide-session
                  :buffer (generate-new-buffer name)
                  :process (codex-ide-test-process-create :live (not dead))
                  :directory temporary-file-directory
                  :query-only query-only :status status)))
    (push session codex-ide--sessions)
    session))

(ert-deftest codex-ide-global-status-counts-requests-and-idle-chats ()
  (codex-ide-test-with-global-status
   (codex-ide-test--global-status-session "idle" "idle")
   (codex-ide-test--global-status-session "running" "running")
   (codex-ide-test--global-status-session "query" "idle" :query-only t)
   (codex-ide-test--global-status-session "dead-process" "idle" :dead t)
   (let ((killed (codex-ide-test--global-status-session "killed" "idle")))
     (kill-buffer (codex-ide-session-buffer killed)))
   (let ((approval (codex-ide-test--global-status-session "approval" "approval")))
     (codex-ide-approvals-data-add approval 1 'command nil)
     (codex-ide-approvals-data-add approval 2 'elicitation nil :status 'active)
     (codex-ide-approvals-data-add approval 3 'command nil :status 'accepted)
     (should (equal (substring-no-properties
                     (or (codex-ide-global-status--indicator) ""))
                    " Appr[2] Idle[1]")))))

(ert-deftest codex-ide-global-status-follows-session-events-and-cleans-up ()
  (codex-ide-test-with-global-status
   (let ((session (codex-ide-test--global-status-session "chat" "running"))
         (updates 0))
     (cl-letf (((symbol-function 'force-mode-line-update)
                (lambda (&optional _all) (cl-incf updates))))
       (codex-ide-global-status-mode 1)
       (codex-ide-global-status-mode 1)
       (should (= (length global-mode-string) 2))
       (should-not (eval (cadr (cadr global-mode-string))))
       (setq updates 0)
       (codex-ide--set-session-status session "idle" 'turn-completed)
       (should (> updates 0))
       (should (equal (substring-no-properties
                       (eval (cadr (cadr global-mode-string))))
                      " Idle[1]"))
       (codex-ide--set-session-status session "running" 'turn-started)
       (should-not (eval (cadr (cadr global-mode-string))))
       (codex-ide-global-status-mode -1)
       (should (equal global-mode-string '("clock")))
       (setq updates 0)
       (codex-ide--set-session-status session "idle" 'turn-completed)
       (should (= updates 0))))))

(ert-deftest codex-ide-global-status-visits-oldest-approval-and-returns-after-resolution ()
  (codex-ide-test-with-global-status
   (let* ((origin (generate-new-buffer "origin"))
          (idle (codex-ide-test--global-status-session "idle" "idle"))
          (newer (codex-ide-test--global-status-session "newer" "approval"))
          (older (codex-ide-test--global-status-session "older" "approval"))
          (old-buffer (codex-ide-session-buffer older))
          (codex-ide-display-buffer-pop-up-action '((display-buffer-same-window))))
     (with-current-buffer old-buffer (insert "before\nrequest\nafter"))
     (codex-ide-approvals-data-add newer 2 'command nil :created-at '(2 0))
     (codex-ide-approvals-data-add
      older 1 'command nil :created-at '(1 0)
      :view (list :start-marker (with-current-buffer old-buffer (copy-marker 8))))
     (switch-to-buffer origin)
     (insert "original position")
     (let ((position (point)))
       (call-interactively #'codex-ide-visit-ready)
       (should (eq (window-buffer) old-buffer))
       (should (= (point) 8))
       (codex-ide-approvals-data-resolve older 1 'accepted)
       (codex-ide-approvals-data-resolve newer 2 'accepted)
       (codex-ide--set-session-status older "running")
       (call-interactively #'codex-ide-visit-ready)
       (should (eq (window-buffer) origin))
       (should (= (point) position))
       (should-not codex-ide-global-status--origin)
       (call-interactively #'codex-ide-visit-ready)
       (should (eq (window-buffer) (codex-ide-session-buffer idle)))))))

(ert-deftest codex-ide-global-status-idle-click-chooses-chat-and-returns ()
  (codex-ide-test-with-global-status
   (let* ((origin (generate-new-buffer "origin"))
          (first (codex-ide-test--global-status-session "first" "idle"))
          (second (codex-ide-test--global-status-session "second" "idle"))
          (codex-ide-display-buffer-pop-up-action '((display-buffer-same-window)))
          (indicator (codex-ide-global-status--indicator))
          (command (lookup-key (get-text-property 1 'local-map indicator)
                               [mode-line mouse-1])))
     (switch-to-buffer origin)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt choices &rest _args)
                  (should (member (buffer-name (codex-ide-session-buffer first)) choices))
                  (should (member (buffer-name (codex-ide-session-buffer second)) choices))
                  (buffer-name (codex-ide-session-buffer second)))))
       (call-interactively command))
     (should (eq (window-buffer) (codex-ide-session-buffer second)))
     (call-interactively command)
     (should (eq (window-buffer) origin)))))

(ert-deftest codex-ide-global-status-no-ready-chat-preserves-current-buffer ()
  (codex-ide-test-with-global-status
   (codex-ide-test--global-status-session "running" "running")
   (let ((origin (current-buffer)))
     (should-error (codex-ide-visit-ready) :type 'user-error)
     (should (eq (current-buffer) origin)))))

(ert-deftest codex-ide-global-status-approval-click-does-not-visit-idle-chat ()
  (codex-ide-test-with-global-status
   (let* ((approval (codex-ide-test--global-status-session "approval" "approval"))
          (_idle (codex-ide-test--global-status-session "idle" "idle"))
          (codex-ide-display-buffer-pop-up-action '((display-buffer-same-window))))
     (codex-ide-approvals-data-add approval 1 'command nil)
     (let* ((indicator (codex-ide-global-status--indicator))
            (command (lookup-key (get-text-property 1 'local-map indicator)
                                 [mode-line mouse-1])))
       (call-interactively command)
       (should (eq (window-buffer) (codex-ide-session-buffer approval)))))))

(ert-deftest codex-ide-global-status-destroyed-session-disappears ()
  (codex-ide-test-with-global-status
   (let ((session (codex-ide-test--global-status-session "idle" "idle")))
     (codex-ide-global-status-mode 1)
     (should (codex-ide-global-status--indicator))
     (codex-ide--teardown-session session)
     (should-not (codex-ide-global-status--indicator))
     (should codex-ide-global-status-mode))))

(ert-deftest codex-ide-global-status-killed-origin-releases-marker ()
  (codex-ide-test-with-global-status
   (let ((origin (generate-new-buffer "origin"))
         (codex-ide-display-buffer-pop-up-action '((display-buffer-same-window))))
     (codex-ide-test--global-status-session "idle" "idle")
     (switch-to-buffer origin)
     (codex-ide-visit-ready)
     (kill-buffer origin)
     (should-error (codex-ide-visit-ready) :type 'user-error)
     (should-not codex-ide-global-status--origin)
     (should-not codex-ide-global-status--target))))

(ert-deftest codex-ide-global-status-disable-releases-return-marker ()
  (codex-ide-test-with-global-status
   (codex-ide-test--global-status-session "idle" "idle")
   (codex-ide-global-status-mode 1)
   (let ((codex-ide-display-buffer-pop-up-action '((display-buffer-same-window))))
     (codex-ide-visit-ready)
     (let ((origin codex-ide-global-status--origin))
       (should (marker-buffer origin))
       (codex-ide-global-status-mode -1)
       (should-not (marker-buffer origin))
       (should-not codex-ide-global-status--origin)))))

(provide 'codex-ide-global-status-tests)
;;; codex-ide-global-status-tests.el ends here
