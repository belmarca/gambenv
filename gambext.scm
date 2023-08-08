(define-macro (define-command name help-string handler)
  (let ((##handler-id (string->symbol (string-append "##gsi-option-" (symbol->string name))))
        (handler-id (string->symbol (string-append (symbol->string name) "-handler")))
        (help-id (string->symbol (string-append (symbol->string name) "-help"))))
    `(begin
       (define (,help-id)
         (println ,help-string))
       (define ,handler-id ,handler)
       (define (,##handler-id args)
         (if (pair? args)
             (,handler-id args)
             (,help-id)))
       (##gsi-option-handlers-set!
        (##cons (##cons (quote ,name) ,##handler-id) ##gsi-option-handlers)))))

(define dot-gambit-path
  (cddddr (##string-split-at-char-reversed (path-expand "~~lib") #\/)))

(define (join-strings strings delim)
  (join-strings-reverse (reverse strings) delim))
(define (join-strings-reverse strings delim)
  (fold (lambda (x y)
          (if (= 0 (string-length y))
              x
              (string-append x delim y))) "" strings))

(define (available-gambit-versions)
  (let* ((p (open-directory (join-strings-reverse
                             (cons "versions" dot-gambit-path) "/")))
         (versions (read-all p)))
    (close-input-port p)
    versions))

(define (set-gambit-current-version! version)
  ;; NOTE: Any directory inside ~/.gambit/versions is going to be valid.
  ;; This is useful if you want to work on the Gambit compiler itself.
  ;; You can put your development version there, under an alias, and it
  ;; immediately becomes available to you.
  (let ((current (join-strings-reverse (cons "current" (cons "versions" dot-gambit-path)) "/")))
    (delete-file current)
    (create-symbolic-link
     (join-strings-reverse (cons version (cons "versions" dot-gambit-path)) "/")
     current)))

(define (select-gambit-version version-string)
  (let* ((versions (available-gambit-versions))
         (version (member version-string versions)))
    (if version
        ;; Version is already available
        ;; TODO: Make sure this succeeds
        (begin
          (set-gambit-current-version! version-string)
          (exit 0))
        ;; TODO: It might be a git tag, compile?
        (println "Version not available. (TODO: Compile it?)"))))

(define-command select
  "gsi -select VERSION\n\nChange the current Gambit version to VERSION."
  (lambda (args)
    ;; TODO: Better handling of course. 'list' is a subcommand, thus
    ;; can't be used as version alias.
    (if (equal? "list" (car args))
        (println (string-append "Available Gambit versions:\n\n"
                                (join-strings (available-gambit-versions) "\n")))
        (select-gambit-version (car args)))))
