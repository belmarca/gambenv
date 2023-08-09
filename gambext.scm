(define-macro (define-command name help-message handler)
  (let ((##handler-id (string->symbol (string-append "##gsi-option-" (symbol->string name))))
        (handler-id (string->symbol (string-append (symbol->string name) "-handler")))
        (help-id (string->symbol (string-append (symbol->string name) "-help"))))
    `(begin
       (define (,help-id)
         (println (string-concatenate ,help-message)))
       (define ,handler-id ,handler)
       (define (,##handler-id args)
         (if (pair? args)
             (,handler-id args)
             (begin (,help-id) (exit 1))))
       (##gsi-option-handlers-set!
        (##cons (##cons (quote ,name) ,##handler-id) ##gsi-option-handlers)))))

;; Misc
(define (join-strings strings delim)
  (fold-right (lambda (x y)
                (if (= 0 (string-length y))
                    x
                    (string-append x delim y))) "" strings))

;; Paths and options
(define dot-gambit-path
  (join-strings
   (reverse (cddddr (##string-split-at-char-reversed (path-expand "~~lib") #\/)))
   "/"))
(define (dot-gambit path)
  (string-append dot-gambit-path path))

(define (available-gambit-versions)
  (let* ((p (open-directory (dot-gambit "/versions")))
         (versions (read-all p)))
    (close-input-port p)
    versions))

(define (set-current-gambit-version! version)
  (let ((current (dot-gambit "/current")))
    (delete-file current)
    (create-symbolic-link
     (dot-gambit (string-append "/versions/" version))
     current)
    (config-set! 'current-gambit-version version)))

(define (select-gambit-version version-string)
  (let* ((versions (available-gambit-versions))
         (version (member version-string versions)))
    (if version
        ;; Version is already available
        ;; TODO: Make sure this succeeds
        (begin
          (set-current-gambit-version! version-string)
          (exit 0))
        ;; TODO: It might be a git tag, compile?
        (println "Version '" version "' not available."))))

(define (yes-or-no? msg)
  (display (string-append msg " (y/n): "))
  (let ((answer (read)))
    (and (symbol? answer)
         (or (eq? 'y answer)
             (eq? 'Y answer)))))

(define (config key)
  (let ((kv (assoc key gambdev-config)))
    (when (not kv)
      (println "Key '" key "' not found in gambdev config.")
      (exit 1))
    (cdr kv)))

(define (remove-gambit-version version)
  ;; Must be existing version and not the one created by gambdev
  (when (or (equal? version (config 'gambdev-gambit-version))
            (not (member version (available-gambit-versions))))
    (println "Cannot remove this version.")
    (exit 1))
  (let ((path (dot-gambit (string-append "/versions/" version))))
    (if (file-exists? path)
        (let ((consent (yes-or-no? (string-append "Do you want to remove the '" version "' Gambit version?"))))
          (if consent
              (begin
                (delete-file-or-directory path #t)
                ;; Reset current to gambdev-default if needed
                (if (equal? version (current-gambit-version))
                    (let ((default-version (assoc 'gambdev-gambit-version gambdev-config)))
                      (when (not default-version)
                        (println "Key " ,key " not present in gambdev config.")
                        (exit 1))
                      (set-current-gambit-version! default-version)))
                ;; Remove symlinks in .gambit/bin
                (let ((gsi (string-append "gsi@" version))
                      (gsc (string-append "gsc@" version)))
                  (delete-file (dot-gambit (string-append "/bin/" gsi)))
                  (delete-file (dot-gambit (string-append "/bin/" gsc))))
                (println "Removed Gambit version '" version "'."))))
        (begin
          (println "Version '" version "' not found.")
          (exit 1)))))

(define (config-set! key val)
  (with-output-to-file (dot-gambit "/config.scm")
    (lambda ()
      (let ((obj (assoc key gambdev-config)))
        (when (not obj)
          (println "Key " ,key " not present in gambdev config.")
          (exit 1))
        (set-cdr! obj val)
        (##pretty-print gambdev-config)))))

(define (list-gambit-versions)
  (println
   (string-append
    "Available Gambit versions:\n\n"
    (join-strings (available-gambit-versions) "\n"))))

(define gambdev-config
  (call-with-input-file "/home/belmarca/.gambit/config.scm"
    (lambda (p) (read p))))

(define (used-version-name? version)
  (member name (available-gambit-versions)))

(define (allowed-version-name? name)
  (not (member name '("list" "new" "rm" "info"))))

(define (build-gambit-version args)
  (when (not (>= (length args) 3))
    (println "Wrong number of arguments.")
    (exit 1))
  ;; NOTE: Consider using version-alias instead of version
  (let* ((version (car args))
         (hash-or-tag (cadr args))
         (configure-options (join-strings (cddr args) " "))
         (source-dir (dot-gambit (string-append "/sources/" version)))
         (install-dir (dot-gambit (string-append "/versions/" version)))
         (userlib-dir (string-append install-dir "/userlib"))
         (gsi (string-append "gsi@" version))
         (gsc (string-append "gsc@" version)))
    ;; TODO: Check version is a tag or a hash directly in the git repo
    (when (not (allowed-version-name? version))
      (println "Version '" version "' is not allowed.")
      (exit 1))
    (when (member version (available-gambit-versions))
      (println "Version '" version "' already exists.")
      (exit 1))
    (build-gambit-version* version hash-or-tag configure-options
                           source-dir install-dir userlib-dir
                           gsi gsc)
    ))

(define (build-gambit-version* version hash-or-tag configure-options
                               source-dir install-dir userlib-dir
                               gsi gsc)
  (define command-string
    (string-concatenate
     (list "cd " source-dir
           " && "
           "git checkout " hash-or-tag
           " && "
           "./configure "
           " --prefix=" install-dir
           " --enable-default-runtime-options=~~userlib=" userlib-dir " "
           configure-options
           " && "
           ;; NOTE: Accept make config
           "make -j8 && make install")))

  ;; Ask for consent
  (if (not (yes-or-no? (string-append "You are about to build Gambit '" hash-or-tag "' aliased to '" version "'\n"
                                      "with configure options '" configure-options "'\n"
                                      "Do you wish to proceed?")))
      (begin
        (println "Aborting.")
        (exit 1)))

  ;; Create the directories
  (create-directory install-dir)
  (create-directory userlib-dir)
  ;; Copy the ~/.gambit/versions/gambit code to the source dir
  (copy-directory (dot-gambit "/sources/gambit/") source-dir)
  ;; Configure, compile and install

  ;; FIXME: Getting an error even if the build completes.
  ;; The issue is in 'make install'
  ;; (let ((success (shell-command build-and-install)))
  ;;   (unless (not (= 0 success))
  ;;     (println "There was an error building Gambit.")
  ;;     (error 1))
  ;; Just build without check for now
  (shell-command command-string)

  ;; Create @version symlinks
  (create-symbolic-link (string-append install-dir "/bin/gsi")
                        (dot-gambit (string-append "/bin/" gsi)))
  (create-symbolic-link (string-append install-dir "/bin/gsc")
                        (dot-gambit (string-append "/bin/" gsc)))
  ;; gambext symlink
  (create-symbolic-link (dot-gambit "/gambext.scm")
                        (string-append install-dir "/lib/gambext")))

(define (copy-directory from to)
  (define code
    (shell-command (string-append "cp -r " from " " to)))
  (when (not (= code 0))
    ;; NOTE: shell-command is probably going to raise first
    (println "Error copying directory.")
    (exit 1)))

(define-command select
  '(
    "gsi -select VERSION_ALIAS\n"
    "  Change the current Gambit version to VERSION_ALIAS.\n\n"

    "gsi -select list\n"
    "  List the installed Gambit versions.\n\n"

    "gsi -select remove VERSION_ALIAS\n"
    "  Removes the installed Gambit version VERSION_ALIAS.\n\n"

    "gsi -select new [REPO_URL] ALIAS HASH_OR_TAG CONFIGURE_OPTIONS\n"
    "  Compile and install a new Gambit version aliased to ALIAS. Gambit\n"
    "  will be checked out at HASH_OR_TAG and configured with CONFIGURE_OPTIONS.\n"
    "  If REPO_URL is present and is either in HTTP, HTTPS or SSH, REPO_URL\n"
    "  is cloned instead of the default Gambit repo."
    )
  (lambda (args)
    ;; TODO: Better arg handling of subcommands?
    (cond
     ((equal? "list" (car args))
      (list-gambit-versions))
     ((equal? "new" (car args))
      (build-gambit-version (cdr args)))
     ((equal? "remove" (car args))
      (remove-gambit-version (cadr args)))
     (else (select-gambit-version (car args))))))
