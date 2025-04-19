(require 'transient)
(defvar bench-buffer-name "*bench-log*")

(defun bench-log-clear()
  (interactive)
  (if
      (get-buffer bench-buffer-name)
      (with-current-buffer bench-buffer-name
	(setq buffer-read-only nil)
	(delete-region 1 (+ 1 (buffer-size)))
	(setq buffer-read-only t)
	)
    )
  )

(define-derived-mode frappe-bench-mode special-mode "Frappe Bench"
  ;; filter function failes if buffer is read-only
  (setq buffer-read-only nil)
  ;; (setq buffer-read-only t)
  )

(defun handle-control-codes(proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert (subst-char-in-string ?\^M ?\^J string))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))
	)))
  )

;; (subst-char-in-string ?\^M ?\^J mig-out)

(defun bench-dispatch-command(&rest args)
  (let* ((program "bench"))
    (message "args: %s" args)
    (make-process :name "*bench-command*"
		  :buffer bench-buffer-name
		  :command (append '("bench") args)
		  :filter 'handle-control-codes)
    (when (not (string-equal (buffer-name (current-buffer)) bench-buffer-name))
      (switch-to-buffer-other-window bench-buffer-name))
    (frappe-bench-mode)
    ))


(defun bench-list-apps()
  (interactive)
  (bench-dispatch-command "list-apps"))

(defun bench-list-sites()
  (interactive)
  (bench-dispatch-command "list-sites"))

(defun bench-build(prefix-args)
  (interactive "P")
  (let* ((args (transient-args transient-current-command))
	 (app (transient-arg-value "--app=" args))
	 (force (transient-arg-value "--force" args))
	 (verbose (transient-arg-value "--verbose" args))
	 (production (transient-arg-value "--production" args))
	 (save-meta (transient-arg-value "--save-metafiles" args))
	 (arguments (list "build")))

    (when app
      (setq arguments (append arguments (list "--app") (list app))))
    (when force
      (setq arguments (append arguments (list "--force"))))
    (when verbose
      (setq arguments (append arguments (list "--verbose"))))
    (when production
      (setq arguments (append arguments (list "--production"))))
    (when save-meta
      (setq arguments (append arguments (list "--save-meta"))))
    (apply 'bench-dispatch-command arguments)
    ))

(transient-define-prefix bench-build-prefix()
  ["Build"
   ("-a" "App" "--app=")
   ("-p" "Production" "--production")
   ("-f" "Force" "--force")
   ("-v" "Verbose" "--verbose")
   ("-s" "Save Meta Files" "--save-metafiles")
   ("b" "Build" bench-build)
   ]
  )

(transient-define-suffix bench-create-site(prefix-args)
  (interactive "P")
  (let* ((site-name (transient-arg-value "--site=" (transient-args transient-current-command)))
	 (pass (read-passwd "password: ")))
    (bench-dispatch-command "new-site" "--admin-password" pass "--db-root-username" "root" "--db-root-password" pass site-name)
    )
  )

(defun bench-init-dir(prefix-args)
  (interactive "P")
  (let* ((args (transient-args transient-current-command))
	 (bench-dir (transient-arg-value "--init=" args))
	 (frappe-path (transient-arg-value "--frappe-path=" args))	 
	 (branch (transient-arg-value "--frappe-branch=" args))
	 (arguments (list "init"))
	 )
    (when frappe-path
      (setq arguments (append arguments (list "--frappe-path") (list frappe-path))))
    (when branch
      (setq arguments (append arguments (list "--frappe-branch") (list branch))))
    (when (transient-arg-value "--dev" args)
      (setq arguments (append arguments (list "--dev"))))
    (when (transient-arg-value "--verbose" args)
      (setq arguments (append arguments (list "--verbose"))))
    (when bench-dir
      (setq arguments (append arguments (list bench-dir))))
    (apply 'bench-dispatch-command arguments)
    )
  )

(transient-define-prefix bench-init-prefix()
  ["Initialize New Bench in Directory"
   ["Paths"
    ("fp" "Path to frappe repo" "--frappe-path=" transient-read-directory)
    ("fb" "Frappe Branch" "--frappe-branch=")
    ]
   ["Toggles"
    ("D" "Enable Developer Mode" "--dev")
    ("v" "Verbose" "--verbose")
    ]
   [
    ("d" "Initialize Bench in" "--init=" transient-read-directory)
    ("i" "Init" bench-init-dir)
    ]
   ]
  )


(transient-define-suffix bench-drop-site(prefix-args)
  (interactive "P")
  (let* ((site-name (transient-arg-value "--site=" (transient-args transient-current-command)))
	 (pass (read-passwd "Password: ")))
    (bench-dispatch-command "drop-site" site-name "--force" "--no-backup" "--db-root-username" "root" "--db-root-password" pass))
  )

(transient-define-suffix bench-use-site(prefix-args)
  (interactive "P")
  (let* ((site-name (transient-arg-value "--site=" (transient-args transient-current-command))))
    (bench-dispatch-command "use" site-name))
  )


(transient-define-suffix bench-migrate(prefix-args)
  (interactive "P")
  (let* ((site-name (transient-arg-value "--site=" (transient-args transient-current-command)))
	 )
    (bench-dispatch-command "migrate"))
  )

(transient-define-suffix bench-update(prefix-args)
  (interactive "P")
  (let* ((args (transient-args transient-current-command))
	 (command-arguments '()))
    (progn 
      (when (transient-arg-value "--force" args)
	(setq command-arguments (append command-arguments (list "--force"))))
      (when (transient-arg-value "--pull" args)
	(setq command-arguments (append command-arguments (list "--pull"))))
      (when (transient-arg-value "--no-compile" args)
	(setq command-arguments (append command-arguments (list "--no-compile"))))

      (apply 'bench-dispatch-command "update" command-arguments)
      )
    )
  )

(defun bench-send-interupt()
  (interactive)
  (interrupt-process bench-buffer-name))


(defvar-keymap frappe-bench-mode-map
  "n" #'next-line
  "p" #'previous-line
  "c" #'bench-log-clear
  "g" #'bench-dispatch
  "C-c C-c" #'bench-send-interupt)

(defvar-local bench-selected-site "")

(defun bench-site-describe()
  (format "--site=%s" (propertize bench-selected-site 'face 'transient-value)))


(transient-define-prefix bench-site-prefix()
  "Site commands"
  ["Site"
   ;; ("-s" bench-set-site :description bench-site-describe :prompt "")
   ("-s" "Site Name" "--site=" :prompt "Site:" :always-read t :allow-empty nil :init-value (lambda (obj) (oset obj value "demo1")))
   ("c" "Create Site" bench-create-site)
   ("d" "Drop Site" bench-drop-site)
   ("u" "Use" bench-use-site)]
  )

(transient-define-prefix bench-update-prefix()
  "Update"
  ["Update"
   ("-f" "Force" "--force")
   ("-p" "Pull" "--pull")
   ("-nc" "No Compile" "--no-compile")
   ("u" "Update" bench-update)
   ]
  )


(transient-define-suffix bench-get-app(prefix-args)
  (interactive "P")
  (let* ((args (transient-args transient-current-command))
	 (branch (transient-arg-value "--branch" args))
	 (overwrite (transient-arg-value "--overwrite" args))
	 (app-url (transient-arg-value "--url=" args) )
	 (arguments '()))
    (progn 
      (when branch
	(setq arguments (append command-arguments '("--branch") '(branch))))
      (when overwrite
	(setq arguments (append command-arguments '("--overwrite"))))
      (setq arguments (append arguments (list app-url)))
      (apply 'bench-dispatch-command "get-app" arguments)
      )
    )
)

(transient-define-prefix bench-get-app-prefix()
  "Get App popup prefix"
  ["Get App"
   ("-b" "Branch" "--branch=")
   ("-o" "Overwrite" "--overwrite")
   ("u" "App Url" "--url=")
   ("g" "Get" bench-get-app)
   ]
  )

(defun bench-help()
  (interactive)
  (bench-dispatch-command "--help"))

(defun bench-set-site(site-name)
  (interactive (list (read-string "Site: ")))
  (setq bench-selected-site site-name))

(transient-define-prefix bench-dispatch()
  "Frappe Bench Commands"
  ["Bench Commands"
   ["Init"
    ("i" "Initialize New Bench"  bench-init-prefix)
    ]
   ["List"
    ("l s" "List Sites" bench-list-sites)
    ("l a" "List Apps" bench-list-apps)
    ]
   ["Site"
    ("s" "Site Commands" bench-site-prefix)
    ]
   ["Modify"
    ("b" "Build Assets" bench-build-prefix)
    ("m" "Migrate site" bench-migrate)
    ("u" "Update site" bench-update-prefix)]
   ]
  ["Get"
   ("a" "Get App" bench-get-app-prefix)
   ]
  )
