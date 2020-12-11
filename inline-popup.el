

(defvar inlup--current-popup nil "The overlay of the current inline popup.")
(defvar inlup--current-lines nil)
(defvar inlup--current-index nil)
(defvar inlup--invokinkg-command nil)
(defvar inlup--current-footer nil)


(defface diff-hl-show-hunk-added-face  '((t (:foreground "green"))) "Face for added lines" :group 'diff-hl-show-hunk-group)
(defface diff-hl-show-hunk-deleted-face  '((t (:foreground "red" :strike-through t))) "Face for deleted lines" :group 'diff-hl-show-hunk-group)


(make-variable-buffer-local 'inlup--current-popup)
(make-variable-buffer-local 'inlup--current-lines)
(make-variable-buffer-local 'inlup--current-index)
(make-variable-buffer-local 'inlup--current-footer)
(make-variable-buffer-local 'inlup--invokinkg-command)

(defun inlup--splice (list offset length)
  (butlast
   (nthcdr offset list)
   (- (length list) length offset)))

(defun inlup--compute-height ()
  (/ (window-height) 2))

(defun inlup--compute-content-lines (lines index window-size)
  (let* ((len (length lines))
         (window-size (min window-size len))
         (index (min index (- len window-size))))
    (inlup--splice lines index window-size)))

(defun inlup--compute-header (width &optional header)
  (let* ((scroll-indicator (if (eq inlup--current-index 0) "   " " ⬆ "))
         (header (or header ""))
         (width (- width (length header) (length scroll-indicator)))
         (line (propertize (concat (inlup--separator width) header scroll-indicator ) 'face '(:underline t))))
    (concat "\n" line "\n") ))

(defun inlup--compute-footer (width &optional footer)
  (let* ((scroll-indicator (if (>= inlup--current-index (- (length inlup--current-lines) (inlup--compute-height))) "   "     " ⬇ "))
         (footer (or footer ""))
         (new-width(- width (length footer) (length scroll-indicator)))
         (blank-line (propertize (inlup--separator width) 'face '(:underline t)))
         (line (propertize (concat (inlup--separator new-width) footer scroll-indicator))))
    (concat "\n" blank-line "\n" line)))

(defun inlup--separator (width &optional sep)
  (let ((sep (or sep ?\s)))
    (make-string width sep)))

(defun inlup--compute-popup-str (lines index window-size)
  (let* ((magic-adjust-that-works-on-my-pc 6)
         (width (- (window-body-width) magic-adjust-that-works-on-my-pc))
         (footer "(n)Next  (p)Previous  (q)Close")
         (content-lines (inlup--compute-content-lines lines index window-size))
         (header (inlup--compute-header width))
         (footer (inlup--compute-footer width footer)))
    (concat header (string-join content-lines  "\n" ) footer)))

(defun inlup-show (lines &optional footer propertize-line-function point buffer)
  (let* ((the-point (or point (point-at-eol)))
         (the-buffer (or buffer (current-buffer)))
         (overlay (make-overlay the-point the-point the-buffer)))
    (overlay-put overlay 'phantom t)
    (overlay-put overlay 'inlup t)
    (setq inlup--current-popup overlay)

    (setq inlup--current-lines lines)
    (setq inlup--current-footer footer)
    (setq inlup--propertize-line-function propertize-line-function)
    (setq inlup--invokinkg-command this-command)
    (inlup-transient-mode 1)
    (inlup-scroll 0)
    overlay))

(defun inlup-scroll (index)
  (when inlup--current-popup
    (setq inlup--current-index (max 0 (min index (- (length inlup--current-lines) (inlup--compute-height)))))
    (let* ((str (inlup--compute-popup-str inlup--current-lines inlup--current-index (inlup--compute-height))))
      (overlay-put inlup--current-popup 'after-string str))))

(defun inlup--hide-all ()
  (interactive)
  (when inlup-transient-mode
    (inlup-transient-mode -1))
  (setq inlup--current-popup nil)
  (let* ((all-overlays (overlays-in (point-min) (point-max)))
         (overlays (cl-remove-if-not (lambda (o)(overlay-get o 'inlup)) all-overlays)))
    (dolist (o overlays)
      (delete-overlay o))))

(defun inlup-hide()
  (interactive)
  (when inlup-transient-mode
    (inlup-transient-mode -1))
  (when inlup--current-popup
    (delete-overlay inlup--current-popup)
    (setq inlup--current-popup nil)))

(defun inlup--check-click-outside-popup(event)
  (interactive "e")
  (inlup-hide))

(defun inlup--popup-down()
  (interactive)
  (inlup-scroll (1+ inlup--current-index) ))

(defun inlup--popup-up()
  (interactive)
  (inlup-scroll (1- inlup--current-index) ))

(defun inlup--popup-pagedown()
  (interactive)
  (inlup-scroll (+ (inlup--compute-height) inlup--current-index) ))

(defun inlup--popup-pageup()
  (interactive)
  (inlup-scroll (- (inlup--compute-height) inlup--current-index) ))

(defvar inlup-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") #'inlup--popup-pageup)
    (define-key map (kbd "M-v") #'inlup--popup-pageup)
    (define-key map (kbd "<next>") #'inlup--popup-pagedown)
    (define-key map (kbd "C-v") #'inlup--popup-pagedown)
    (define-key map (kbd "<up>") #'inlup--popup-up)
    (define-key map (kbd "C-p") #'inlup--popup-up)
    (define-key map (kbd "<down>") #'inlup--popup-down)
    (define-key map (kbd "C-n") #'inlup--popup-down)
    (define-key map (kbd "C-g") #'inlup-hide)
    (define-key map [escape] #'inlup-hide)
    (define-key map (kbd "q") #'inlup-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") #'inlup--popup-up)
    (define-key map (kbd "<wheel-up>") #'inlup--popup-up)
    (define-key map (kbd "<mouse-5>") #'inlup--popup-down)
    (define-key map (kbd "<wheel-down>") #'inlup--popup-down)
    
    (define-key map (kbd "p") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "n") #'diff-hl-show-hunk-next)
    (define-key map (kbd "C-x v {") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "C-x v }") #'diff-hl-show-hunk-next)
    map)
  "Keymap for command `inlup-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")

(defun inlup--ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing an inline popup."
  (member command `(,inlup--invokinkg-command handle-switch-frame)))


(defun inlup--post-command-hook ()
  "Called each time a command is executed."
  (let ((allowed-command (or
                          (string-match-p "inlup-" (symbol-name this-command))
                          (inlup--ignorable-command-p this-command))))
    (unless allowed-command
      (inlup-hide))))

(define-minor-mode inlup-transient-mode
  "Temporal minor mode to control an inline popup"
  :global nil
  (remove-hook 'post-command-hook #'inlup--post-command-hook t)
  (when inlup-transient-mode
    (add-hook 'post-command-hook #'inlup--post-command-hook 0 t)))


(defun inlup--inlup-hide ()
  (interactive)
  (inlup-hide))


(setq diff-hl-show-hunk-function #'diff-hl-show-hunk-inlup)
(defun diff-hl-show-hunk-inlup (buffer line)
  "Implementation to show the hunk in a inline popup.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  
  (inlup-hide)
  (setq inlup--hide-function #'inlup--inlup-hide)
  
  (let* ((lines (split-string (with-current-buffer buffer (buffer-string)) "[\n\r]+" ))
         (line (max 0 (- line 1)))
         (propertized-lines (mapcar (lambda (l)
                                      (let ((face (cond ((string-prefix-p "+" l) 'diff-hl-show-hunk-added-face)
                                                        ((string-prefix-p "-" l) 'diff-hl-show-hunk-deleted-face))))
                                        (if face
                                            (propertize l 'face face)
                                          l)))
                                    lines))
         (clicked-line (propertize (nth line lines) 'face 'diff-hl-show-hunk-clicked-line-face)))
    (setcar (nthcdr line propertized-lines) clicked-line)
    (inlup-show propertized-lines nil #'diff-hl-show-hunk-inlup-propertize-function)
    (inlup-scroll line))
  t)

(defun inlup--test()
  (interactive)
  (inlup-show  (list "INICIO" "Hola" "Que" "Tal" "yo" "bien" "gracias" "y" "usted" "pues" "aqui" "ando" "FIN")))

