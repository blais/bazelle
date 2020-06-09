;; Emacs support for running Bazel commands.
;; (This should probably be merged in emacs-bazel-mode eventually.)
;;
;; Author: Martin Blais <blais@furius.ca>
;; Copyright: Apache

(require 'bazel-util)
(require 'bazel-build)

(defun bazelle-command-on-current (command)
  "Launch an interactive compilation on the target of the current buffer."
  (let ((current (or (buffer-file-name)
                     (and dired-directory (directory-file-name dired-directory)))))
    (compile (format "cd %s && %s %s %s"
                     (or (bazel-util-workspace-root current)
                         (error "Could not find workspace."))
                   bazel-command command (bazel-build--read-target)))))

(defun bazelle-build ()
  "Build the target for the current buffer."
  (interactive)
  (bazelle-command-on-current "build"))

(defun bazelle-test ()
  "Run tests on the target for the current buffer."
  (interactive)
  (bazelle-command-on-current "test"))

(defun bazelle-test-at-point ()
  "Run tests on only the test around the cursor."
  (interactive)
  ;; TODO(blais): Implement test selection.
  (bazelle-command-on-current "test"))

(defun bazelle-run ()
  "Run tests on the target for the current buffer."
  (interactive)
  (bazelle-command-on-current "run"))

(provide 'bazelle)
