;; Emacs support for running Bazel commands.
;; (This should probably be merged in emacs-bazel-mode eventually.)
;;
;; Author: Martin Blais <blais@furius.ca>
;; Copyright: Apache

(defvar bazelle-command "bazel"
  "Bazel executable to run for subprocesses.")

(defun string-rstrip (str)
  "Strips the whitespace at the end of string STR."
  (string-match "[ \t\n]*\\'" str)
  (substring str 0 (match-beginning 0)))

(defun bazelle-is-workspace (&optional dirname)
  "Predicate for whether the given (or current buffer) dirname
is within a bazel workspace. Returns the workspace root or nil."
  (let* ((startdir (or dirname (buffer-file-name)))
         (rootdir (or (locate-dominating-file startdir "WORKSPACE")
                      (locate-dominating-file startdir "WORKSPACE.bazel"))))
    (when rootdir
      (directory-file-name rootdir))))

(defun bazelle-call-process (&rest args)
  "Run a bazel subcommand. Return stdout as string."
  (let ((stderr-file (make-temp-file "bazelle-stderr")))
    (unwind-protect
        (string-rstrip
         (with-output-to-string
           (let ((status
                  (apply #'call-process bazelle-command
                         nil (list standard-output stderr-file) nil args)))
             (if (/= status 0)
                 (with-temp-buffer
                   (insert-file-contents stderr-file)
                   (error "Error running command: %s" (buffer-string)))))))
      (if (file-exists-p stderr-file)
	  (delete-file stderr-file)))))

(defun bazelle-target-for-filename (filename)
  "Get the target which includes the given filename."
  (let* ((default-directory (file-name-directory filename))
         ;; Resolve label for file with bazel query.
         (fullname (bazelle-call-process
                    "query" (file-name-nondirectory filename))))
    ;; Produce target the file-label is in using bazel query.
    (bazelle-call-process "query"
                          (format "attr('srcs', %s, %s:*)"
                                  fullname
                                  (car (split-string fullname ":"))))))

(defun bazelle-command-on-current (command)
  "Launch an interactive compilation on the target of the current buffer."
  (condition-case err
      (let* ((target (bazelle-target-for-filename (buffer-file-name)))
             (compile-command
              (format "cd %s && %s %s %s"
                      (bazelle-is-workspace)
                      bazelle-command command target)))
        (call-interactively 'compile))))

;; TODO(blais): Add compilation-read with completion to this (there may be more
;; than one matching target, use the set of targets in the file's package).
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
  (bazelle-command-on-current "test"))

(defun bazelle-run ()
  "Run tests on the target for the current buffer."
  (interactive)
  (bazelle-command-on-current "run"))

(provide 'bazelle)
