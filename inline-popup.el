




(defvar inlup--current-popup nil "The overlay of the current inline popup.")
(defvar inlup--current-lines nil)
(defvar inlup--current-index nil)
(defvar inlup--invokinkg-command nil)
(defvar inlup--current-footer nil)
(defvar inlup-window-size 5 "The heigth of the inline popup content.")

(make-variable-buffer-local 'inlup--current-popup)
(make-variable-buffer-local 'inlup--current-lines)
(make-variable-buffer-local 'inlup--current-index)
(make-variable-buffer-local 'inlup--current-footer)
(make-variable-buffer-local 'inlup--invokinkg-command)

(defun inlup--splice(list offset length)
  (let ((ret (butlast
              (nthcdr offset list)
              (- (length list) length offset))))
    ret))

(defun inlup--compute-content-lines (lines index window-size)
  (let* ((len (length lines))
         (window-size (min window-size len))
         (index (min index (- len window-size)))
         (ret (inlup--splice lines index window-size)))
    ret))

(defun inlup--compute-header (width &optional header)
  (let* ((scroll-indicator (if (eq inlup--current-index 0) "   " " ⬆ "))
         (header (or header ""))
         (width (- width (length header) (length scroll-indicator))))
    (concat (inlup--separator width) header scroll-indicator)))

(defun inlup--compute-footer (width &optional footer)
  (let* ((scroll-indicator (if (>= inlup--current-index (- (length inlup--current-lines) inlup-window-size)) "   "     " ⬇ "))
         (footer (or footer ""))
         (width (- width (length footer) (length scroll-indicator))))
    (concat (inlup--separator width) footer scroll-indicator)))

(defun inlup--separator (width &optional sep)
  (let ((sep (or sep ?\s)))
    (make-string width sep)))

(defun inlup--compute-popup-str (lines index window-size)
  (let* ((magic-adjust-that-works-on-my-pc 6)
         (width (- (window-body-width) magic-adjust-that-works-on-my-pc))
         (footer "(n)Next  (p)Previous  (q)Close")
         (content-lines (inlup--compute-content-lines lines index window-size))
         (start (concat "\n" (propertize (inlup--compute-header width) 'face '(:underline t)) "\n"))
         (end (concat "\n" (propertize  (inlup--compute-footer width footer) 'face '(:overline t)))))
    (concat start (string-join content-lines  "\n" ) end)))

(defun inlup-show (lines &optional footer point buffer)
  (let* ((the-point (or point (point-at-eol)))
         (the-buffer (or buffer (current-buffer)))
         (overlay (make-overlay the-point the-point the-buffer)))
    (overlay-put overlay 'phantom t)
    (overlay-put overlay 'inlup t)
    (setq inlup--current-popup overlay)
    (setq inlup--current-lines lines)
    (setq inlup--current-footer footer)
    (setq inlup--invokinkg-command this-command)
    (inlup-transient-mode 1)
    (inlup-scroll 0)
    overlay))

(defun inlup-scroll (index)
  (when inlup--current-popup
    (setq inlup--current-index (max 0 (min index (- (length inlup--current-lines) inlup-window-size))))
    (let* ((str (inlup--compute-popup-str inlup--current-lines inlup--current-index inlup-window-size)))
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
  (inlup-scroll (+ inlup-window-size inlup--current-index) ))

(defun inlup--popup-pageup()
  (interactive)
  (inlup-scroll (- inlup-window-size inlup--current-index) ))

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

(defun inlup-ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing an inline popup."
  (member command `(,inlup--invokinkg-command handle-switch-frame)))


(defun inlup--post-command-hook ()
  "Called each time a command is executed."
  (let ((allowed-command (or
                          (string-match-p "inlup-" (symbol-name this-command))
                          (inlup-ignorable-command-p this-command))))
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
         (clicked-line (propertize (nth line lines) 'face 'diff-hl-show-hunk-clicked-line-face)))
    (setcar (nthcdr line lines) clicked-line)
    (inlup-show lines)
    (inlup-scroll line))
  t)

(defun inlup--test()
  (interactive)
  (inlup-show  (list "INICIO" "Hola" "Que" "Tal" "yo" "bien" "gracias" "y" "usted" "pues" "aqui" "ando" "FIN")))

