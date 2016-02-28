;;-------------------------------------------;;
;; General helper functions for stuff here   ;;
;;-------------------------------------------;;

;; Get the line a particular position is on
(defun pos-line (position)
  (save-excursion                            ; Ensure nothing is affected by moving the cursor
    (goto-char position)                     ; Go to the correct position
    (beginning-of-line)                      ; and the start of the line
    (+ 1 (count-lines 1 (point)))))          ; Count the lines up

;; Get the column a particular position is in
(defun pos-col (position)
  (save-excursion                            ; Ensure nothing is affected by moving the cursor
    (goto-char position)                     ; Go to the correct position
    (current-column)))                       ; Return the current column

;;-------------------------------------------;;
;; Functions for region-cursor minor-mode    ;;
;;-------------------------------------------;;

;; Performs funct on every line of a multi-line cursor
(defun rc-apply (funct)
    (setq col
          (current-column)                   ; The column of the point is where we will insert.

          end-line
          (pos-line (min (window-end)
                         (region-end)))      ; Get the last line of the region

          start-line
          (pos-line (max (window-start)
                         (region-beginning))); Get the first line of the region

          orig-line
          (pos-line (point)))                ; Save the line of the original point

    (goto-line end-line)                     ; Go to the end line of the region
    (move-to-column col)                     ; and the column of the cursor

    (if (eq (current-column) col)            ; If a cursor can be placed in the correct column of
        (funcall funct))                     ; the first line of the region, call funct

    (while (> (pos-line (point))             ; For every line in the region, going upwards
              start-line)                    ; this way we avoid changes on prior lines affecting later ones
      (forward-line -1)
      (move-to-column col)

      (if (eq (current-column) col)          ; If a cursor can be placed in the correct column of each line
          (funcall funct)))                  ; call funct

    (goto-line orig-line)                    ; Go to the original line of the point
    (move-to-column col))                    ; and its column. We cannot use the position as it'd be offset.



;; Squishes a region down to a width of 1. Used after region-insert etc.
;; To make the region look like a multi-line cursor thing.
(defun rc-squish ()
  (interactive)

  (setq cur-col
        (current-column))                    ; Save the column of the point

  (set-mark (save-excursion                  ; Move the mark
              (goto-char (mark))             ; to find where to move it to, go to its current position,
              (move-to-column cur-col)       ; and go to the column of the point.
              (point))))



;; Inserts a single character on every line of a region acting as a multi-line cursor
(defun rc-insert-char (char)
  (interactive "cCharacter to insert?")      ; This could be useful to call via M-x sometime...

  (rc-apply (lambda ()                       ; For every line in the region,
              (let (deactivate-mark)         ; making sure we don't deactivate the mark,
                (insert char))))             ; we insert the character on every line.

  (rc-squish)                                ; Squish the region

  (forward-char))                            ; Go forward a character so we are in front of what we inserted

;; To be called from a keypress when a region is acting as a multiline cursor
;; Takes the keypress that called it and inserts it on every line of the region in the cursor's column
(defun rc-insert-command ()
  (interactive)
  (when (region-active-p)                    ; Ensure we are dealing with an actual region
    (setq char
          (concat (this-command-keys)))

    (when (string-match "[[:print:]]" char)  ; Make sure the character is printable
      (rc-insert-char char))                 ; Insert the keypresses that called this function

    (when (string= "" char)                ; If the character is enter,
      (deactivate-mark))))                   ; we get rid of the mark



;; Causes the region to act as a multi-line cursor and delete the characters in the column behind the cursor
(defun rc-backspace ()
  (interactive)

  (when (region-active-p)                    ; Ensure we actually have a region
    (setq orig-mode delete-active-region     ; Save the original mode of delete-active-region
          delete-active-region nil)          ; Make it do we do not delete everything in the region

    (backward-char)                          ; Go backwards

    (rc-apply (lambda ()
                (let (deactivate-mark        ; Ensure we don't kill the region
                      delete-active-region)
                  (delete-char 1))))         ; Delete the charater in front of the cursor on each line

    (rc-squish)                              ; Squish the region into a multi-line cursor looking thing

    (setq delete-active-region orig-mode)))  ; Reset delete-active-region to its original mode (probably t)

;; Kills all current multi-line cursor overlays
(defun kill-rc-overlays ()
  (dolist (overlay rc-overlays)              ; For every current overlay
    (delete-overlay overlay))                ; delete the overlay, so it stops displaying

  (setq rc-overlays nil))                    ; Set the list of current overlays to '()

;; Adds a new multi-line cursor overlay at the current point
(defun new-rc-overlay ()
  (setq position                             ; Save the current point
        (point)

        overlay
        (make-overlay (- position 1)         ; Create a new overlay at the current point
                      position))

  (overlay-put overlay 'face 'region-cursor) ; Give the new overlay its 'face
  (overlay-put overlay 'priority 10)         ; Make sure the new overlay overrides mark-mode's

  (setq rc-overlays                          ; Append the new overlay to the list of current overlays
        (cons overlay
              rc-overlays)))

;; Moves an overlay to the current point
(defun move-rc-overlay (overlay)
  (setq position
        (point))                             ; Save the current point

  (move-overlay overlay
                (- position 1) position)     ; Move the overlay to the new position

  (setq rc-overlays                          ; Append the moved overlay to the list of current overlays
        (cons overlay rc-overlays)))

;; Highlight the multi-line cursor of the current region
(defun rc-highlight ()
  (when (region-active-p)                    ; Ensure we have a region

    (setq cur-col                            ; We want to try and put the cursor in the column to the right
          (+ 1 (pos-col (point)))            ; of the current column. This makes sure that there's a character
                                             ; where we draw the overlay
          unreused-overlays rc-overlays      ; Make a list of the overlays we can still reuse

          rc-overlays nil)                   ; Reset the list of current overlays

    (rc-apply                                ; For every line in the multi-line cursor
     (lambda ()
       (move-to-column cur-col)

       (when (eq (pos-col (point))           ; Before we draw an overlay, make sure there's a character
                 cur-col)                    ; to draw over

         (if (eq nil unreused-overlays)      ; If there are no more reusable overlays,
             (new-rc-overlay)                ; make a new one

           (move-rc-overlay                  ; Otherwise, move the next reusable overlay,
            (car unreused-overlays))

           (setq unreused-overlays           ; and remove it from the list of remaining reusable
                 (cdr unreused-overlays)))))); overlays

    (dolist (overlay unreused-overlays)      ; For every remaining reusable overlay
      (delete-overlay overlay))))            ; Kill it


;; A new face for the position of a multi-line pointer in a region
(defface region-cursor
  '((t :background "color-163"))
  "Face for multi-line cursor")

;; A variable for keeping track of the highlighted overlays of a multi-line cursor
(defvar rc-overlays nil)

;; A keymap for the new minor-mode
(defvar rc-keymap
  '(keymap
    (remap keymap
           (c-electric-backspace
            . rc-backspace)

           (backward-delete-char-untabify    ; Map (usually)backspace key to rc-backspace
            . rc-backspace)

           (self-insert-command              ; Remap any character insertion to rc-insert-command
            . rc-insert-command))))

;; Actually define our minor mode
(define-minor-mode region-cursor-mode
  "A minor mode where the region acts as a tall cursor"
  :keymap
  rc-keymap)
(rc-highlight)

;; Hooks for our minor-mode
(add-hook 'region-cursor-mode-hook
          (lambda ()
            (when region-cursor-mode
              (add-hook
               'post-command-hook
               'rc-highlight))

            (when (not region-cursor-mode)
              (remove-hook
              'post-command-hook
              'rc-highlight)

              (kill-rc-overlays))))

;; Hooks to enable and disable our minor mode with normal mark activation
(add-hook 'activate-mark-hook
          (lambda ()
            (if (not region-cursor-mode)
                (call-interactively
                 'region-cursor-mode))))

(add-hook 'deactivate-mark-hook
          (lambda ()
            (if region-cursor-mode
                (call-interactively
                 'region-cursor-mode))))


;;-------------------------------------------;;
;; Other functions changing region behavior  ;;
;;-------------------------------------------;;

;; This is convenient. Also things don't work without it
(rectangle-mark-mode t)

;; Shift the current region by an amount of lines and columns

(defun region-shift (lines cols)
  (interactive
   "nMove down #lines:\nnMove #chars: ")

  (when (region-active-p)                    ; Ensure we have an actual region
    (setq old-point (point))                 ; Save the original position of the cursor

    (goto-line                               ; Go to the new equivilent position of
     (+ lines (pos-line (mark))))            ; the mark
    (move-to-column
     (+ cols  (pos-col  (mark))))

    (set-mark-command nil)                   ; Set this as the new mark

    (goto-line                               ; Go to the cursor's new position
     (+ lines (pos-line old-point)))
    (move-to-column
     (+ cols  (pos-col  old-point)))))

;; Bind different shift directions to keys
(define-key rectangle-mark-mode-map (kbd "ESC <up>")    (lambda () (region-shift -1  0)))
(define-key rectangle-mark-mode-map (kbd "ESC <down>")  (lambda () (region-shift  1  0)))
(define-key rectangle-mark-mode-map (kbd "ESC <left>")  (lambda () (region-shift  0 -1)))
(define-key rectangle-mark-mode-map (kbd "ESC <right>") (lambda () (region-shift  0  1)))

;; Squishes a region to a width of 1 and justifies it right
(defun region-snap (direction)
  (when (and (region-active-p)               ; Ensure we have an active region to play with
             rectangle-mark-mode)            ; ensure it's also rectangular.

    (setq mark-col                           ; Save the column of the mark
          (pos-col (mark))

          point-col                          ; and the point
          (pos-col (point)))

    (if (or (and (eq direction 'left)        ; If we're snapping to the left
                 (> point-col mark-col))     ; and the point is to the left of the mark
            (and (eq direction 'right)       ; or we're snapping right
                 (< point-col mark-col)))    ; and the point is to the right of the mark

        (move-to-column mark-col)            ; we can just move the point to the mark's column

      (set-mark (save-excursion              ; Otherwise, we need to move the mark. To get its new position,
                  (goto-char (mark))         ; we need to move a pretend point to the mark's current position
                  (move-to-column point-col) ; then move the point to the mark's new column
                  (point))))))               ; and get the point's position.


;; Bind different snap directions to keys
(define-key rectangle-mark-mode-map (kbd "ESC <end>")   (lambda () (region-snap 'right)))
(define-key rectangle-mark-mode-map (kbd "ESC <home>")  (lambda () (region-snap 'left)))

;; Display the sze of the currently selected region. Updates when the last
;; command was acursor move.
(defun region-stats ()
  (interactive)

  (if (and (region-active-p)                 ; Check that a valid region is selected.
           (or
            (eq this-command 'previous-line) ; Check that the last command was a move.
            (eq this-command 'next-line)
            (eq this-command 'right-char)
            (eq this-command 'left-char)))
  (call-interactively
   'count-words-region)))                    ; The gorramn command.


;; Hooks to enable and disable region-stats with mark activation
(add-hook 'activate-mark-hook
          (lambda ()
            (add-hook
             'post-command-hook
             'region-stats)))

(add-hook 'deactivate-mark-hook
          (lambda ()
            (remove-hook
             'post-command-hook
             'region-stats)))

;;-------------------------------------------;;
;; General configuration stuff               ;;
;;-------------------------------------------;;

(setq tetris-score-file                      ; Tetris is important for productivity
      "~/.emacs.d/tetrisscores"

      c-default-style                        ; Linux style is important,
      "linux"                                ; does fuck up #defines tho...

      debug-on-error                         ; Just so we can figure out when things catch fire
      t

;;    debug-ignored-errors                   ; Uncomment for every goddamn error to produce tracebacks etc.
;;    nil

      delete-active-region                   ; We can always C-x u
      t)

(column-number-mode t)                       ; HOW DO WE ALIGN OTHERWISE ;-;

(set-face-attribute 'show-paren-match nil
                    :background "#999"
                    :foreground "#fff")      ; Gray brackets :D

(setq show-paren-delay 0)                    ; Why do you even do that by default ;-;
(show-paren-mode 1)                          ; This is entirely necessary if using either grace's code or lisp

(setq whitespace-style
      '(face trailing tabs tab-mark))        ; Dunno what these do exactly,
(global-whitespace-mode 1)                   ; but they seem important

(setq-default indent-tabs-mode nil             ; Tabs master race
              tab-width 4)                   ; Seriously, 8 spaces is massive, why would you ever use

(setq c-basic-offset 4
	  cperl-indent-level 4)
;(defvaralias 'c-basic-offset     'tab-width) ; it. 4 is so, so much better.
;(defvaralias 'cperl-indent-level 'tab-width)

(add-hook 'html-mode-hook
(add-hook 'emacs-lisp-mode
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))))

(add-hook 'javascript-mode-hook
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq python-indent 4)
            (setq tab-width 4))))

(global-set-key (kbd "M-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "C-e")   'exchange-point-and-mark)

(global-set-key "\eOA" [up])
(global-set-key "\e[A" [up])
(global-set-key "\eOB" [down])
(global-set-key "\e[B" [down])
(global-set-key "\eOC" [right])
(global-set-key "\e[C" [right])
(global-set-key "\eOD" [left])
(global-set-key "\e[D" [left])
