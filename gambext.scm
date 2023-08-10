(define-macro (@doc . args)
  `(void))

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
  (@doc "Join a list of strings with a delimiter.")
  (fold-right
   (lambda (x y)
     (if (= 0 (string-length y))
         x
         (string-append x delim y))) "" strings))

(define (yes-or-no? msg)
  (@doc "Ask for user consent.")
  (display (string-append msg " (y/n): "))
  (let ((answer (read)))
    (and (symbol? answer)
         (or (eq? 'y answer)
             (eq? 'Y answer)))))

(define (config key)
  (@doc "Getter for gambdev-config. 'key' is a symbol.")
  (let ((kv (assoc key gambdev-config)))
    (when (not kv)
      (println "Key '" key "' not found in gambdev config.")
      (exit 1))
    (cdr kv)))

(define (copy-directory from to)
  (define success
    (shell-command (string-append "cp -r " from " " to)))
  (when (not (= success 0))
    ;; NOTE: shell-command is probably going to raise first in case
    ;; of error.
    (println "Error copying directory.")
    (exit 1)))

;; The .gambit path on the users' system
(define .gambit
  (join-strings
   (reverse (cddddr (##string-split-at-char-reversed (path-expand "~~lib") #\/)))
   "/"))

;; NOTE: Cas où @doc non nécessaire
(define (path . args)
  (join-strings args "/"))

(define (.gambit/ f)
  (path .gambit f))

;; gambdev files and directories
(define .gambit/bin        (.gambit/ "bin"))
(define .gambit/current    (.gambit/ "current"))
(define .gambit/sources    (.gambit/ "sources"))
(define .gambit/versions   (.gambit/ "versions"))
(define .gambit/config.scm (.gambit/ "config.scm"))

(define gambdev-config
  (call-with-input-file .gambit/config.scm
    (lambda (p) (read p))))

;; Helpers
(define (available-gambit-versions)
  (let* ((p (open-directory .gambit/versions))
         (versions (read-all p)))
    (close-input-port p)
    versions))

(define (config-set! key val)
  (with-output-to-file .gambit/config.scm
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

(define (used-version-name? version)
  (member name (available-gambit-versions)))

(define (allowed-alias? alias)
  (not (member alias '("list" "new" "remove"))))

(define (valid-hash-or-tag? alias hash-or-tag)
  ;; Must be either a commit hash or a tag
  (= 0 (car (shell-command
             (string-append "git -C " (path .gambit/sources alias)
                            " rev-parse " hash-or-tag) #t))))

;; Handlers
(define (set-current-gambit-version! version-alias)
  ;; Changing versions is just changing the 'current' symlink
  (delete-file .gambit/current)
  (create-symbolic-link
   (path .gambit/versions version-alias)
   .gambit/current)
  (config-set! 'current-gambit-version version-alias))

(define (select-gambit-version version-alias)
  (when (not (member version-alias (available-gambit-versions)))
    (println "Version alias '" version-alias "' not available."))
  (set-current-gambit-version! version-alias)
  (exit 0))

(define (remove-gambit-version version-alias)
  (define remove-version-msg
    (string-append "Do you want to remove the '" version-alias "' Gambit version?"))
  (define remove-sources-msg
    (string-append "Do you want to remove the '" version-alias "' Gambit sources?"))

  ;; Must be existing version and not the one created by gambdev
  (when (not (member version-alias (available-gambit-versions)))
    (println "Version '" version-alias "' does not exist.")
    (exit 1))
  (when (equal? version-alias (config 'gambdev-gambit-version))
    (println "Cannot remove version '" version-alias "' as it was installed by gambdev.")
    (exit 1))

  ;; Removing a version is just deleting a directory, updating the current
  ;; version if required and remove some symlinks.
  (let ((install-dir (path .gambit/versions version-alias))
        (source-dir (path .gambit/sources version-alias)))

    (when (not (file-exists? install-dir))
      (println "Version '" version-alias "' not found."))

    ;; Removing the installed version is mandatory
    (when (not (yes-or-no? remove-version-msg))
      (println "Aborting.")
      (exit 0))
    (delete-file-or-directory install-dir #t)

    ;; But a user could keep the sources.
    (when (yes-or-no? remove-sources-msg)
      (delete-file-or-directory source-dir #t))

    (when (equal? version-alias (config 'current-gambit-version))
      ;; The version was current
      (let ((default-version (assoc 'gambdev-gambit-version gambdev-config)))
        (when (not default-version)
          ;; This should ideally never happen...
          (println "Key " key " not present in gambdev config.")
          (exit 1))
        (set-current-gambit-version! default-version)))
    ;; Remove symlinks in .gambit/bin
    (delete-file (path .gambit/bin (string-append "gsi@" version-alias)))
    (delete-file (path .gambit/bin (string-append "gsc@" version-alias)))
    (println "Removed Gambit version '" version-alias "'.")))

(define (build-gambit-version args)
  (when (not (>= (length args) 3))
    (println "Wrong number of arguments.")
    (exit 1))
  (let* ((alias             (car args))
         ;; TODO: Add optional URL
         (hash-or-tag       (cadr args))
         (configure-options (join-strings (cddr args) " "))
         (source-dir        (path .gambit/sources alias))
         (install-dir       (path .gambit/versions alias))
         (userlib-dir       (path install-dir "userlib"))
         (gsi               (string-append "gsi@" alias))
         (gsc               (string-append "gsc@" alias)))
    (when (not (allowed-alias? alias))
      (println "Version '" alias "' is not allowed.")
      (exit 1))
    (when (member alias (available-gambit-versions))
      (println "Version '" alias "' already exists.")
      (exit 1))
    (build-gambit-version* alias hash-or-tag configure-options
                           source-dir install-dir userlib-dir
                           gsi gsc)))

(define (build-gambit-version* alias hash-or-tag configure-options
                               source-dir install-dir userlib-dir
                               gsi gsc)
  ;; Handle rpath
  (define shared
    (string-contains configure-options "--enable-shared"))
  (define rpath (string-append "-Wl,-rpath," install-dir "/lib"))
  (define rpath-flag
    (if shared
        (string-append "--enable-ldflags-gambuild=" rpath
                       " LDFLAGS=" rpath)
        ""))

  (define command-string
    (string-concatenate
     (list
      "cd " source-dir
      " && "
      "git checkout " hash-or-tag
      " && "
      "./configure "
      " --prefix=" install-dir
      " --enable-default-runtime-options=~~userlib=" userlib-dir
      " " configure-options
      " " rpath-flag
      " && "
      ;; NOTE: Accept make flags?
      "make -j8 && make install")))

  ;; Ask for consent
  (when (not (yes-or-no?
              (string-append "You are about to build Gambit '" hash-or-tag "' aliased to '" alias "'\n"
                             "with configure options '" configure-options "'\n"
                             "Do you wish to proceed?")))
    (println "Aborting.")
    (exit 1))

  ;; Create the directories
  (create-directory install-dir)
  (create-directory userlib-dir)

  ;; Copy the ~/.gambit/versions/gambit code to the source dir
  (copy-directory (path .gambit/sources "gambit/") source-dir)

  ;; Unfortunately, this check can only happen late
  (when (not (valid-hash-or-tag? alias hash-or-tag))
    (println "Hash or tag '" hash-or-tag "' does not exist.")
    (exit 1))

  ;; Configure, compile and install
  ;; FIXME: 'make install' can produce errors. For now, don't check
  ;; for success.
  ;; FIXME: Figure out why make -j8 is not taking 8 cores
  (shell-command command-string)

  ;; Create @version and gambext symlinks
  (create-symbolic-link (string-append install-dir "/bin/gsi")
                        (string-append .gambit/bin "/" gsi))
  (create-symbolic-link (string-append install-dir "/bin/gsc")
                        (string-append .gambit/bin "/" gsc))
  (create-symbolic-link (path "gambext.scm")
                        (string-append install-dir "/lib/gambext"))

  (println "Gambit version '" alias "' has been installed."))

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


;; TODO
;; rajouter détection de --enable-shared et modification du rpath
