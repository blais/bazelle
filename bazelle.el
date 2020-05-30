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
  (let* ((startdir (or dirname
                       (buffer-file-name)
                       (and dired-directory (directory-file-name dired-directory))))
         (rootdir (or (locate-dominating-file startdir "WORKSPACE")
                      (locate-dominating-file startdir "WORKSPACE.bazel"))))
    (when rootdir
      (directory-file-name rootdir))))

(defun bazelle-call-process (&rest args)
  "Run a bazel subcommand. Return stdout as string."
  ;; Note: The current working directory of the subprocess is set to the current
  ;; buffer's value of default-directory.
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

(defun bazelle-build-target-for-file (filename)
  "Get the list of targets which includes the given filename."
  (let* ((default-directory (file-name-directory filename))
         ;; Resolve label for file with bazel query.
         (fullname (bazelle-call-process
                    "query" (file-name-nondirectory filename))))
    ;; Produce target the file-label is in using bazel query.
    (let ((cmd (format "attr('srcs', %s, %s:*)" fullname
                       (car (split-string fullname ":")))))
      (split-string (bazelle-call-process "query" cmd)))))

(defun bazelle-build-target-for-directory (dirname)
  "Get the list of targets under the given directory name."
  (let* ((default-directory dirname)
         (results (split-string (bazelle-call-process "query" "kind('.*rule', ':*')")))
         (package (car (split-string (car results) ":"))))
    (append (list (concat package ":all")) results)))

(defun bazelle-build-target-for-directory-or-filename (file-or-dir)
  "Get the list of targets under the given file or directory name."
  (if (file-directory-p file-or-dir)
      (bazelle-build-target-for-directory file-or-dir)
    (bazelle-build-target-for-file file-or-dir)))

(defun bazelle-read-target (&optional filename)
  "Read a target name for the given or current file or dired directory name."
  (let* ((targets (bazelle-build-target-for-directory-or-filename
                   (or
                    ;; A given filename.
                    filename
                    ;; The buffer filename.
                    (buffer-file-name)
                    ;; Open on a BUILD file.
                    (let* ((file-name (buffer-file-name)))
                      (when (and file-name
                                 (string= (file-name-nondirectory file-name) "BUILD"))
                        (directory-file-name (file-name-directory file-name))))
                    ;; Open on a dired directory.
                    (and dired-directory (directory-file-name dired-directory))))))
    (completing-read "Target: " targets nil nil (car targets))))

(defun bazelle-command-on-current (command)
  "Launch an interactive compilation on the target of the current buffer."
  (condition-case err
      (let* ((compile-command
              (format "cd %s && %s %s %s"
                      (or (bazelle-is-workspace) (error "Could not find workspace."))
                      bazelle-command command (bazelle-read-target))))
        (add-to-list 'compile-history compile-command)
        (compile compile-command))))

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
