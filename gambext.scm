(define-macro (@doc . args)
  `(void))


;; Test for the existence of global variables
;; > (##global-var-ref (##make-global-var 'foo))
;; #!unbound
;; > (##global-var-ref (##make-global-var 'car))
;; #<procedure #2 car>
;; > (##make-global-var 'car)
;; car
;; > (##make-global-var 'foo)
;; foo
;; >


;; TODO: Refactor into procedure
(define-macro (define-command name help-message handler)
  (let ((##handler-id (string->symbol (string-append "##gsi-option-" (symbol->string name))))
        (handler-id (string->symbol (string-append (symbol->string name) "-handler")))
        (help-id (string->symbol (string-append (symbol->string name) "-help"))))
    `(begin
       (define (,help-id)
         (println (string-concatenate ,help-message)))
       (define ,handler-id ,handler)
       (define (,##handler-id args)
         (,handler-id args))
       (##gsi-option-handlers-set!
        (##cons (##cons (quote ,name) ,##handler-id) ##gsi-option-handlers)))))

;; Misc
(define (yes-or-no? msg)
  ;; NOTE: Il existe apparemment une fonction similaire dans Gambit
  ;; pas trouvé sous 'dialog ou 'prompt
  (@doc "Ask for user consent.")
  (display (string-append msg " (y/n): "))
  (let ((answer (read)))
    (and (symbol? answer)
         (or (eq? 'y answer)
             (eq? 'Y answer)))))

(define (config key)
  (@doc "Getter for gambenv-config. 'key' is a symbol.")
  (let ((kv (assoc key gambenv-config)))
    (when (not kv)
      (println "Key '" key "' not found in gambenv config.")
      (exit 1))
    (cdr kv)))

;; NOTE: Not portable
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
  ;; FIXME: Not portable in 4.9.3
  (string-concatenate
   (reverse (cddddr (##string-split-at-char-reversed (path-expand "~~lib") #\/)))
   "/"))

(define (path . args)
  (fold (lambda (x y) (path-expand x y)) "" args))

(define (.gambit_env/ f)
  (path .gambit f))

;; gambenv files and directories
(define .gambit_env/bin        (.gambit_env/ "bin"))
(define .gambit_env/current    (.gambit_env/ "current"))
(define .gambit_env/sources    (.gambit_env/ "sources"))
(define .gambit_env/versions   (.gambit_env/ "versions"))
(define .gambit_env/config.scm (.gambit_env/ "config.scm"))

(define gambenv-config
  (call-with-input-file .gambit_env/config.scm
    (lambda (p) (read p))))

;; Helpers
(define (available-gambit-versions)
  ;; directory-files
  (let* ((p (open-directory .gambit_env/versions))
         (versions (read-all p)))
    (close-input-port p)
    versions))

(define (config-set! key val)
  (with-output-to-file .gambit_env/config.scm
    (lambda ()
      (let ((obj (assoc key gambenv-config)))
        (when (not obj)
          (println "Key " ,key " not present in gambenv config.")
          (exit 1))
        (set-cdr! obj val)
        (##pretty-print gambenv-config)))))

(define (list-gambit-versions)
  (println
   (string-append
    "Available Gambit versions:\n\n"
    (string-concatenate (available-gambit-versions) "\n"))))

(define (used-version-name? version)
  (member name (available-gambit-versions)))

(define (allowed-alias? alias)
  (not (member alias '("list" "new" "remove"))))

;; module _git gambit
(define (valid-hash-or-tag? alias hash-or-tag)
  ;; Must be either a commit hash or a tag
  (= 0 (car (shell-command
             (string-append "git -C " (path .gambit_env/sources alias)
                            " rev-parse " hash-or-tag) #t))))

;; Handlers
(define (set-current-gambit-version! version-alias)
  ;; TODO: Not global
  ;; Changing versions is just changing the 'current' symlink
  (delete-file .gambit_env/current)
  (create-symbolic-link
   (path .gambit_env/versions version-alias)
   .gambit_env/current)
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

  ;; Must be existing version and not the one created by gambenv
  (when (not (member version-alias (available-gambit-versions)))
    (println "Version '" version-alias "' does not exist.")
    (exit 1))
  (when (equal? version-alias (config 'gambenv-gambit-version))
    (println "Cannot remove version '" version-alias "' as it was installed by gambenv.")
    (exit 1))

  ;; Removing a version is just deleting a directory, updating the current
  ;; version if required and remove some symlinks.
  (let ((install-dir (path .gambit_env/versions version-alias))
        (source-dir (path .gambit_env/sources version-alias)))

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
      (let ((default-version (assoc 'gambenv-gambit-version gambenv-config)))
        (when (not default-version)
          ;; This should ideally never happen...
          (println "Key " key " not present in gambenv config.")
          (exit 1))
        (set-current-gambit-version! default-version)))
    ;; Remove symlinks in .gambit_env/bin
    (delete-file (path .gambit_env/bin (string-append "gsi@" version-alias)))
    (delete-file (path .gambit_env/bin (string-append "gsc@" version-alias)))
    (println "Removed Gambit version '" version-alias "'.")))

(define (build-gambit-version args)
  ;; TODO: Handle case without configure options
  (define nargs (length args))
  (cond
   ((= nargs 2)
    (build-gambit-version-2 args))
   ((>= nargs 3)
    (build-gambit-version-3 args))
   (else
    (println "Wrong number of arguments.")
    (exit 1))))

(define (build-gambit-version-2 args)
  ;; User hasn't provided configure options
  (let* ((alias             (car args))
         (hash-or-tag       (cadr args))
         (source-dir        (path .gambit_env/sources alias))
         (install-dir       (path .gambit_env/versions alias))
         (userlib-dir       (path install-dir "userlib"))
         (gsi               (string-append "gsi@" alias))
         (gsc               (string-append "gsc@" alias)))
    (when (not (allowed-alias? alias))
      (println "Version '" alias "' is not allowed.")
      (exit 1))
    (when (member alias (available-gambit-versions))
      (println "Version '" alias "' already exists.")
      (exit 1))
    (build-gambit-version! alias hash-or-tag ""
                           source-dir install-dir userlib-dir
                           gsi gsc)))

(define (build-gambit-version-3 args)
  (let* ((alias             (car args))
         ;; TODO: Add optional URL
         (hash-or-tag       (cadr args))
         (configure-options (string-concatenate (cddr args) " "))
         (source-dir        (path .gambit_env/sources alias))
         (install-dir       (path .gambit_env/versions alias))
         (userlib-dir       (path install-dir "userlib"))
         (gsi               (string-append "gsi@" alias))
         (gsc               (string-append "gsc@" alias)))
    (when (not (allowed-alias? alias))
      (println "Version '" alias "' is not allowed.")
      (exit 1))
    (when (member alias (available-gambit-versions))
      (println "Version '" alias "' already exists.")
      (exit 1))
    (build-gambit-version! alias hash-or-tag configure-options
                           source-dir install-dir userlib-dir
                           gsi gsc)))

(define (build-gambit-version! alias hash-or-tag configure-options
                               source-dir install-dir userlib-dir
                               gsi gsc)
  ;; Handle rpath when built with --enable-shared
  (define shared
    (##string-contains configure-options "--enable-shared"))
  (define rpath (string-append "-Wl,-rpath," install-dir "/lib"))
  (define rpath-flag
    (if shared
        (string-append "--enable-ldflags-gambuild=" rpath
                       " LDFLAGS=" rpath)
        ""))

  (define (cmd . args)
    (define cd (string-append "cd " source-dir " && "))
    (string-concatenate (cons cd args)))

  (define git-checkout (cmd "git checkout " hash-or-tag))
  (define configure (cmd "./configure"
                         " --prefix=" install-dir
                         " --enable-default-runtime-options=~~userlib=" userlib-dir
                         " " configure-options
                         " " rpath-flag))
  ;; NOTE: Currently does not work on --enable-multiple-threaded-vms
  (define make (cmd "make -j8"))
  (define install (cmd "make install"))

  (define (run cmd)
    (println "========== run " cmd)
    (when (not (= 0 (shell-command cmd)))
      (println "There was an error when executing '" cmd "'.")
      (exit 1)))

  ;; Ask for consent
  (when (not (yes-or-no?
              (string-append "You are about to build Gambit '" hash-or-tag "' aliased to '" alias "'\n"
                             "with configure options '" configure-options "'\n"
                             "Do you wish to proceed?")))
    (println "Aborting.")
    (exit 1))

  ;; Copy the ~/.gambit_env/versions/gambit code to the source dir
  (copy-directory (path .gambit_env/sources "gambit/") source-dir)

  ;; Unfortunately, this check can only happen late
  (when (not (valid-hash-or-tag? alias hash-or-tag))
    (println "Hash or tag '" hash-or-tag "' does not exist.")
    (delete-file-or-directory source-dir #t)
    (exit 1))

  ;; Create the directories
  (create-directory install-dir)
  (create-directory userlib-dir)

  ;; Configure, compile and install
  (run git-checkout)
  (run configure)
  (run make)
  ;; This one can fail if unmet dependencies, e.g. when generating docs
  (shell-command install)

  ;; Create @version and gambext symlinks
  (create-symbolic-link (string-append install-dir "/bin/gsi")
                        (string-append .gambit_env/bin "/" gsi))
  (create-symbolic-link (string-append install-dir "/bin/gsc")
                        (string-append .gambit_env/bin "/" gsc))
  (create-symbolic-link (.gambit_env/ "gambext.scm")
                        (string-append install-dir "/lib/gambext"))

  (println "Gambit version '" alias "' has been installed."))

(define-command env
  '(
    "gsi -env VERSION_ALIAS\n"
    "  Change the current Gambit version to VERSION_ALIAS.\n\n"

    "gsi -env-remove VERSION_ALIAS\n"
    "  Removes the installed Gambit version VERSION_ALIAS.\n\n"

    )
  (lambda (args)
    (when (not (pair? args))
      (println "Missing ALIAS argument.")
      (exit 1))
    (select-gambit-version (car args))))

(define-command env-list
  '("gsi -env-list\n"
    "  List the installed Gambit versions.\n\n")
  (lambda (args)
    (list-gambit-versions)))

(define-command env-new
  '("gsi -env-new [REPO_URL] ALIAS HASH_OR_TAG CONFIGURE_OPTIONS\n"
    "  Compile and install a new Gambit version aliased to ALIAS. Gambit\n"
    "  will be checked out at HASH_OR_TAG and configured with CONFIGURE_OPTIONS.\n"
    "  If REPO_URL is present and is either in HTTP, HTTPS or SSH, REPO_URL\n"
    "  is cloned instead of the default Gambit repo.")
  build-gambit-version)
