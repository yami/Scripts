; Ifun is a minor mode to find things in a buffer in ido way.
; Originally it is used for finding functions, so the name 'ifun'.
;
; In fact, after I wrote ifun.el, I knew that it is easy to make
; ido works like ifun. O.K. I wrote ifun because I love ido and
; want to know how it works.
;
; The approach used here is exactly same as ido.el, so ifun also
; illustrates how ido works.
;
; To use ifun (with imenu), here is an example to find 'sections' in a file
;
; a section is defined as
;
; >;; ;;;;;;;
; >;; c-mode
; >;; ;;;;;;
;
; put following into your .emacs
;
; (require 'ifun)
; (require 'imenu)
;
; (setq example-imenu-expr 
;    `(("SECTIONS"
;        ,(concat "^;; ;+\n" ";; \\([[:alnum:]_-]+\\):.*\n" ";; ;+\n") 1)))
;
; (ifun-register
;   (make-ifun-rentry
;     :key (kbd "ESC M-s")     ; Use 'ESC M-s' to activate ifun
;     :candidate-find (ifun-make-imenu-find "SECTIONS" 	example-imenu-expr)))
;

(eval-when-compile (require 'cl))
;; -----------------------------------------------------------------
;; data structures
;; -----------------------------------------------------------------

;; registeration entry, provided by user.
(defstruct ifun-rentry
  key                         ;key sequence returned by kbd macro
  candidate-find              ;function to find candidates
  (candidate-text 'car)       ;function to extract name from candidate
  (candidate-mark 'cdr)       ;function to extract mark from candidate
  (prompt "ifun")             ;prompt
  )

;; dynamic entry, created and used by ifun to store command specific
;; runtime information, such as history.
(defstruct ifun-dentry
  )

;; Command context, includes rentry and dentry.
(defstruct ifun-context
  rentry
  dentry)


;; -----------------------------------------------------------------
;; command specific varaibles (and related)
;;
;; Variables belong to one command, such as registeration information,
;; history.
;; -----------------------------------------------------------------

(defvar ifun-prompt nil
  "Prompt string for minibuffer input. See ifun-rentry.")

(defvar ifun-eoinput nil
  "Point where minibuffer input ends and completion info begins.")

(defvar ifun-context nil
  "Active context. When a command is executed, its
  context (history etc.) is loaded and becomes active.")

(defvar ifun-contexts nil
  "All contexts registered. Every command has its own context.")
(make-variable-buffer-local 'ifun-contexts)

(defvar ifun-candidates nil
  "Active candidates. Candidates from active context.")

(defvar ifun-prospects nil
  "Current active prospects.")

(defvar ifun-candidate-text nil
  "Function to get text from candidate.")

(defvar ifun-candidate-mark nil
  "Function to get marker from candidate.")

(defvar ifun-candidate-find nil
  "Function to find candidates")


;; -----------------------------------------------------------------
;; globals
;; -----------------------------------------------------------------
(defvar ifun-minibuffer-map nil
  "The keymap used in minibuffer by ifun.")

(defvar ifun-buffer nil
  "The buffer before we switch to minibuffer.")

(defvar ifun-enable t
  "If true, ifun hooks for minibuffer will take effects.")

(defvar ifun-exit-status nil
  "Exit status from minibuffer.")

(defvar ifun-rescan t
  "Rescan if true")

;; -----------------------------------------------------------------
;; minor mode
;; -----------------------------------------------------------------

;; <1> mode variable
(defvar ifun-mode nil
  "A mode applies ido idea to select functions/definitions in one
  buffer")
(make-variable-buffer-local 'ifun-mode)

;; <2> mode command
(defun ifun-mode (&optional arg)
  (interactive "P")

  ;; toggle this mode on/off
  (setq ifun-mode (if (null arg)
                      (not ifun-mode)
                    (> (prefix-numeric-value arg) 0)))
  (when ifun-mode (ifun-setup-keymap)))
;; </2>

;; <3> add to minor-mode-alist if your want to indicate the minor mode
;; in the mode line. We don't really it here, but for demonstration
;; purpose, we put it here.

(add-to-list 'minor-mode-alist '(ifun-mode " ifun") t)
              
;; </3>


;; <4> Define a keymap for minor mode.

;; <> define a keymap
(defvar ifun-mode-map nil
  "The ifun mode keymap.")

;; <> no keys defined here, since we let user do that always.
(unless ifun-mode-map
  (setq ifun-mode-map (make-sparse-keymap)))
  
;; <> add it to minor-mode-map-alist
(add-to-list 'minor-mode-map-alist (cons 'ifun-mode ifun-mode-map))

;; </4>

;; -----------------------------------------------------------------
;; context managemer
;; -----------------------------------------------------------------

;; add a new context if key is not context manger.
(defun ifun-cmanager-add (key context)
  (if (assoc key ifun-contexts)
      nil
    (add-to-list 'ifun-contexts (cons key context))))

;; get a context by key
(defun ifun-cmanager-get (key)
  (cdr (assoc key ifun-contexts)))

;; TODO remove context by key 
(defun ifun-cmanager-del (key)
  )

;; -----------------------------------------------------------------
;; context switch
;; -----------------------------------------------------------------
(defun ifun-set-context (context)
  (let* ( (rentry (ifun-context-rentry context))
          (dentry (ifun-context-dentry context)) )    
    (setq ifun-context context)
    (fset 'ifun-candidate-find (ifun-rentry-candidate-find rentry))
    (fset 'ifun-candidate-text (ifun-rentry-candidate-text rentry))
    (fset 'ifun-candidate-mark (ifun-rentry-candidate-mark rentry))
    (setq ifun-prompt (ifun-rentry-prompt rentry))))



;; -----------------------------------------------------------------
;; registeration
;; -----------------------------------------------------------------

;; generated a command for key-binding
(defun ifun-gen-command (key)
  (lexical-let ( (context (ifun-cmanager-get key)) )
    (lambda ()       
      (interactive)
      (ifun-set-context context)
      (setq ifun-enable t)              ;TODO, block a command invokation if enable is t
      (setq ifun-buffer (current-buffer))
      
      (setq ifun-candidates (ifun-candidate-find))

      (ifun-init)
      
      (condition-case nil          
          (read-from-minibuffer (concat ifun-prompt ": ") nil ifun-minibuffer-map)
        (quit (setq ifun-exit-status 'quit)))

      (ifun-fini)
      
      (let ( (winner nil) )
        (cond
         ( (eq ifun-exit-status 'take-first)
           (setq winner (car ifun-prospects)))
         ( (eq ifun-exit-status 'take-default)
           (message "take default")))
        
        (when winner
          (goto-char (marker-position (ifun-candidate-mark winner))))
        
        (setq ifun-enable nil)
      ))))

;; add a new command.
(defun ifun-register (rentry)
  (let ( (key (ifun-rentry-key rentry)) )
    (ifun-cmanager-add key (make-ifun-context
                            :rentry rentry
                            :dentry (make-ifun-dentry)))
    (define-key ifun-mode-map key (ifun-gen-command key))))

;; -----------------------------------------------------------------
;; candidates and prospects 
;; -----------------------------------------------------------------
(defun ifun-find-prospects (text)
  "Pick all prospects from candidates w.r.t user's input"
  (let* ( (quoted-text (regexp-quote text))
          (middle-re   (concat ".*" quoted-text ".*"))
          middle-prospects prospects)
    (mapcar #'(lambda (candidate)
                (let ( (ctext (ifun-candidate-text candidate)) )
                  (cond ((string-match middle-re ctext)
                         (push candidate middle-prospects)))
                  )
                t)
            ifun-candidates)
    (when middle-prospects
      (setq prospects (nconc middle-prospects prospects)))
                
  prospects))

;; copied from ido.el
(defun ifun-chop (items elem)
  "Remove all elements before ELEM and put them at the end of ITEMS."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car items))
      (if (equal next elem)
	  (setq ret (append items (nreverse sofar)))
	;; else
	(progn
	  (setq items (cdr items))
	  (setq sofar (cons next sofar)))))
    ret))

(defun ifun-next-prospect ()
  "next prospect in prospects."
  (interactive)
  (if ifun-prospects
      (let ( (next (cadr ifun-prospects)) )
        (setq ifun-prospects (ifun-chop ifun-prospects next))
        (setq ifun-rescan nil))))

(defun ifun-prev-prospect ()
  "previous prospect in prospects."
  (interactive)
  (if ifun-prospects
      (let ( (prev (car (last ifun-prospects))) )
        (setq ifun-prospects (ifun-chop ifun-prospects prev))
        (setq ifun-rescan nil))))


;; -----------------------------------------------------------------
;; display
;; -----------------------------------------------------------------
(defun ifun-generate-display ()
  "Generated display string for prospects."
  (reduce #'(lambda (acc match-text)
              (concat acc " | " match-text))
          ifun-prospects
          :key 'ifun-candidate-text
          :initial-value " "))

;; -----------------------------------------------------------------
;; minibuffer
;; -----------------------------------------------------------------  
(defun ifun-minibuffer-setup ()
  (add-hook 'pre-command-hook 'ifun-minibuffer-tidy nil t)
  (add-hook 'post-command-hook 'ifun-minibuffer-exhibit nil t)
  (setq ifun-candidates (buffer-local-value 'ifun-candidates ifun-buffer))
  (setq ifun-candidate-text (buffer-local-value 'ifun-candidate-text ifun-buffer))
  )            

(defun ifun-minibuffer-exit ()
  (remove-hook 'pre-command-hook 'ifun-minibuffer-tidy)
  (remove-hook 'post-command-hook 'ifun-minibuffer-exhibit))
  
(defun ifun-minibuffer-user-text ()
  (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))

(defun ifun-minibuffer-display (display-text)
  (save-excursion
    (goto-char ifun-eoinput)
    (insert display-text)))
     
(defun ifun-minibuffer-exhibit ()
  (when ifun-enable
    (if ifun-rescan
        (let ( (user-text (ifun-minibuffer-user-text))
               matches display)
          (setq ifun-eoinput (point-max))        
          (setq ifun-prospects (ifun-find-prospects user-text))
          (ifun-minibuffer-display (ifun-generate-display)))
      (ifun-minibuffer-display (ifun-generate-display)))
    (setq ifun-rescan t)))

(defun ifun-minibuffer-tidy ()
  (when ifun-enable
    (delete-region ifun-eoinput (point-max))))


(defun ifun-setup-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") 'ifun-next-prospect)
    (define-key map (kbd "<left>")  'ifun-prev-prospect)
    (define-key map (kbd "C-m") 'ifun-exit-minibuffer)
    (set-keymap-parent map minibuffer-local-map)
    (setq ifun-minibuffer-map map)))
    
(defun ifun-init ()
  (add-hook 'minibuffer-setup-hook 'ifun-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook  'ifun-minibuffer-exit))

(defun ifun-fini ()
  (remove-hook 'minibuffer-setup-hook 'ifun-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook  'ifun-minibuffer-exit))

;; for testing
(defun ifun-main ()
  (interactive)
  (setq ifun-enable t)
  (ifun-set-candidates (cdr (find "SM_PE" g-sm-index :test 'sm-string=)) (lambda (candidate) (car candidate)))
  (let ( (final-text (read-string "ifun: ")) )
    (cond
     ( (eq ifun-exit-status 'take-first)
       (message (ifun-candidate-text (car ifun-prospects))))
    ))
  (setq ifun-enable nil))

;; TODO: support other exit status (if any)
(defun ifun-exit-minibuffer ()
  (interactive)
  (setq ifun-exit-status 'take-first)
  (exit-minibuffer))


;; helpers for imenu

(defun ifun-make-imenu-find (imenu-title imenu-expression)
  (lexical-let ((title imenu-title)
                (expression imenu-expression))
    (lambda ()
      (cdr (find title
                 (save-excursion
                   (imenu--generic-function expression))
                 :test '(lambda (str car-str) (string= str (car car-str))))))))


(provide 'ifun)
