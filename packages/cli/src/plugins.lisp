;;;; packages/cli/src/plugins.lisp — FR-720 plugin architecture.
;;;;
;;;; A small extension mechanism: plugins register REPL commands, compiler-pass
;;;; hooks, and VM instruction definitions through DEFINE-CL-CC-PLUGIN, and
;;;; LOAD-CL-CC-PLUGINS autoloads .lisp plugin files from *PLUGIN-DIRECTORY*.

(in-package :cl-cc/cli)

(defvar *plugin-directory*
  (merge-pathnames "cl-cc/plugins/" (user-homedir-pathname))
  "Directory scanned by LOAD-CL-CC-PLUGINS for .lisp plugin files.")

(defvar *loaded-plugins* (make-hash-table :test #'equal)
  "Plugin-name-string -> plugin metadata plist (:name :description :version).")

(defvar *repl-command-extensions* (make-hash-table :test #'equal)
  "REPL command string -> extension plist (:plugin :command :handler :documentation).")

(defvar *compiler-pass-extensions* (make-hash-table :test #'eq)
  "Phase keyword -> list of compiler-pass plists (:name :handler :phase :plugin).")

(defvar *vm-instruction-extensions* (make-hash-table :test #'eq)
  "Op keyword -> VM instruction plist (:plugin :op :definition :documentation).")

(defvar *loaded-plugin-files* (make-hash-table :test #'equal)
  "Namestring -> T for plugin files already loaded (dedup for LOAD-CL-CC-PLUGINS).")

(defun %plugin-name-string (name)
  (string-downcase (string name)))

(defun %register-plugin (name &key description version
                                   repl-commands compiler-passes vm-instructions)
  "Runtime backend for DEFINE-CL-CC-PLUGIN; registers all extension points."
  (setf (gethash (%plugin-name-string name) *loaded-plugins*)
        (list :name name :description description :version version))
  (dolist (rc repl-commands)
    (destructuring-bind (command handler &key documentation) rc
      (setf (gethash command *repl-command-extensions*)
            (list :plugin name :command command
                  :handler handler :documentation documentation))))
  (dolist (cp compiler-passes)
    (destructuring-bind (pass-name handler &key phase) cp
      (let ((phase (or phase pass-name)))
        (push (list :name pass-name :handler handler :phase phase :plugin name)
              (gethash phase *compiler-pass-extensions*)))))
  (dolist (vi vm-instructions)
    (destructuring-bind (op definition &key documentation) vi
      (setf (gethash op *vm-instruction-extensions*)
            (list :plugin name :op op
                  :definition definition :documentation documentation))))
  name)

(defmacro define-cl-cc-plugin (name &key description version
                                         repl-commands compiler-passes vm-instructions)
  "Register a CL-CC plugin NAME and its extension points.

  :repl-commands    ((command-string handler &key documentation) ...)
  :compiler-passes  ((pass-name handler &key phase) ...)
  :vm-instructions  ((op-keyword definition &key documentation) ...)"
  `(%register-plugin ',name
                     :description ,description
                     :version ,version
                     :repl-commands (list ,@(loop for rc in repl-commands
                                                  collect `(list ,(first rc) ,(second rc)
                                                                 ,@(cddr rc))))
                     :compiler-passes (list ,@(loop for cp in compiler-passes
                                                    collect `(list ,(first cp) ,(second cp)
                                                                   ,@(cddr cp))))
                     :vm-instructions (list ,@(loop for vi in vm-instructions
                                                    collect `(list ,(first vi) ,(second vi)
                                                                   ,@(cddr vi))))))

(defun registered-repl-command (command)
  "Return the extension plist registered for the REPL COMMAND string, or NIL."
  (gethash command *repl-command-extensions*))

(defun registered-vm-instruction (op)
  "Return the extension plist registered for the VM instruction OP keyword, or NIL."
  (gethash op *vm-instruction-extensions*))

(defun registered-compiler-passes (phase)
  "Return the list of compiler-pass extension plists registered for PHASE
in registration order."
  (reverse (gethash phase *compiler-pass-extensions*)))

(defun run-repl-command-extension (line)
  "Dispatch LINE to the handler of its leading REPL command token, passing the
whole LINE. Return T when a matching command extension ran, NIL otherwise."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (end (or (position-if (lambda (c) (member c '(#\Space #\Tab))) trimmed)
                  (length trimmed)))
         (command (subseq trimmed 0 end))
         (entry (gethash command *repl-command-extensions*)))
    (when entry
      (funcall (getf entry :handler) line)
      t)))

(defun load-cl-cc-plugins (&key force)
  "Load every *.lisp file in *PLUGIN-DIRECTORY*, skipping files already loaded
unless FORCE is true. Return the list of files loaded by this call."
  (let ((loaded '()))
    (when (uiop:directory-exists-p *plugin-directory*)
      (dolist (found (directory (merge-pathnames "*.lisp" *plugin-directory*)))
        ;; Report the file relative to *PLUGIN-DIRECTORY* as configured, rather
        ;; than DIRECTORY's truename (which resolves symlinks, e.g. /tmp ->
        ;; /private/tmp), so callers get back the pathname they expect.
        (let* ((file (merge-pathnames (file-namestring found) *plugin-directory*))
               (key (namestring file)))
          (when (or force (not (gethash key *loaded-plugin-files*)))
            (load file)
            (setf (gethash key *loaded-plugin-files*) t)
            (push file loaded)))))
    (nreverse loaded)))
