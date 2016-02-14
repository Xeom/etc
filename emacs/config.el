(setq
 tetris-score-file                           ; Tetris is important for productivity
 "~/.emacs.d/tetrisscores"

 c-default-style                             ; Linux style is important,
 "linux"                                     ; does fuck up #defines tho...

 indent-tabs-mode                            ; TABS MASTER-RACE
 t

 debug-on-error                              ; Just so we can figure out when things catch fire
 t

;; debug-ignored-errors                      ; Uncomment for every goddamn error to produce tracebacks etc.
;; nil

 delete-active-region                        ; We can always C-x u
 t)



(column-number-mode 1)                       ; HOW DO WE ALIGN OTHERWISE ;-;

(set-face-attribute 'show-paren-match nil
                    :background "#999"
                    :foreground "#fff")      ; Gray brackets :D



(setq show-paren-delay 0)                    ; Why do you even do that by default ;-;
(show-paren-mode 1)                          ; This is entirely necessary if using either grace's code or lisp



(setq whitespace-style
      '(face trailing tabs tab-mark))        ; Dunno what these do exactly,
(global-whitespace-mode 1)                   ; but they seem important




(setq-default tab-width 4)                   ; Seriously, 8 spaces is massive, why would you ever use
(defvaralias 'c-basic-offset     'tab-width) ; it. 4 is so, so much better.
(defvaralias 'cperl-indent-level 'tab-width)





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
     (+ cols  (pos-col  old-point)))

    (setq deactivate-mark nil)))             ; Ensure our new region doesn't get killed





;; Shift the region one character up
(defun region-shift-up ()
  (interactive)
  (region-shift -1  0))



;; Shift the region one character down
(defun region-shift-down ()
  (interactive)
  (region-shift  1  0))



;; Shift the region one character to the left.
(defun region-shift-left ()
  (interactive)
  (region-shift  0 -1))



;; Shift the region one character to the right.
(defun region-shift-right ()
  (interactive)
  (region-shift  0  1))





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





;; To be called from a hook - Inserts the calling command key(s) at the start
;; of every line of the current selected region rectangle.
(defun region-apply (funct)
  (setq col
        (current-column)                     ; The column of the point is where we will insert.

        end-line
        (pos-line (min (window-end)
                       (region-end)))        ; Get the last line of the region

        start-line
        (pos-line (max (window-start)
                       (region-beginning)))  ; Get the first line of the region

        orig-pos
        (point))

  (goto-line end-line)                       ; Go to the end line of the region
  (move-to-column col)                       ; and the column of the cursor

  (if (eq (current-column) col)              ; If a cursor can be placed in the correct column of
      (funcall funct))                       ; the first line of the region, call funct

  (while (> (pos-line (point))               ; For every line in the region, going upwards
            start-line)                      ; this way we avoid changes on prior lines affecting later ones
    (forward-line -1)
    (move-to-column col)
    (if (eq (current-column) col)            ; If a cursor can be placed in the correct column of each line
        (funcall funct)))                    ; call funct

  (goto-char orig-pos)





;; Squishes a region down to a width of 1. Used after region-insert etc.
;; To make the region look like a multi-line cursor thing.
(defun region-cursorify ()
  (goto-line end-line)                       ; Go to the last line and column of the user's cursor
  (move-to-column col)
  (set-mark (point))                         ; Set this point as one end of the new region

  (goto-line start-line)
  (move-to-column col)                       ; Go to the first line

  (setq deactivate-mark nil))                ; Don't delete the region after





;; Inserts a single character on every line of a region acting as a multi-line cursor
(defun region-insert-char (char)
  (interactive "cCharacter to insert?")      ; This could be useful to call via M-x sometime...

  (when (eq 1 (length char))                 ; Check that we are indeed inserting a single character

    (region-apply (lambda ()                 ; Insert the character on every line of the multi-line cursor
                    (insert char)))

    (region-cursorify)                       ; Squish the region
    (forward-char)))                         ; Go forward a character so we are in front of what we inserted



;; To be called from a keypress when a region is acting as a multiline cursor
;; Takes the keypress that called it and inserts it on every line of the region in the cursor's column
(defun region-insert ()
  (when (region-active-p)                    ; Ensure we are dealing with an actual region
    (delete-char -1)                         ; Get rid of the character inserted by the keypress
    (region-insert-char
     (concat (this-command-keys)))))         ; Insert the keypresses that called this function





;; Causes the region to act as a multi-line cursor and delete the characters in the column behind the cursor
(defun region-backspace ()
  (interactive)

  (when (region-active-p)                    ; Ensure we actually have a region
    (setq orig-mode delete-active-region)    ; Save the original mode of delete-active-region
    (setq delete-active-region nil)          ; Make it do we do not delete everything in the region

    (backward-char)                          ; Go backwards
    (region-apply (lambda ()
                    (delete-forward-char 1))); Delete the charater in front of the cursor on each line

    (region-cursorify)                       ; Squish the region into a multi-line cursor looking thing

    (setq delete-active-region orig-mode)))  ; Reset delete-active-region to its original mode (probably t)





;; Squishes a region to a width of 1 and justifies it right
(defun region-snap-right ()
  (interactive)
  (when (and (region-active-p)               ; Ensure we have an active region to play with
             rectangle-mark-mode)            ; ensure it's also rectangular.

    (if (> (pos-col (mark))                  ; If the mark is in a greater column
           (pos-col (point)))

        (move-to-column (pos-col (mark)))    ; Put the point in the mark-s column

      (setq old-pos                          ; Otherwise, save the point for later
            (point))

      (goto-char (mark))                     ; Go to the line of the mark,
      (move-to-column (pos-col old-pos))     ; and the column of the point

      (set-mark-command nil)                 ; Set this as the new mark

      (goto-char old-pos))))                 ; Return the point to its original position

;;;; TODO: region-snap-left ;;;;





;; A new face for the position of a multi-line pointer in a region
(defface region-cursor
  '((t :background "color-163"))
  "Face for highlighting the character after a rectangle region")

;; A variable for keeping track of the highlighted overlays of a multi-line cursor
(defvar region-cursor-overlays nil)



;; Kills all current multi-line cursor overlays
(defun kill-region-cursor-overlays ()
  (dolist (overlay region-cursor-overlays)   ; For every current overlay
    (delete-overlay overlay))                ; delete the overlay, so it stops displaying

  (setq region-cursor-overlays nil))         ; Set the list of current overlays to '()



;; Adds a new multi-line cursor overlay at the current point
(defun new-region-cursor-overlay ()
  (setq position                             ; Save the current point
        (point)

        overlay
        (make-overlay (- position 1)         ; Create a new overlay at the current point
                      position))

  (overlay-put overlay 'face 'region-cursor) ; Give the new overlay its 'face
  (overlay-put overlay 'priority 10)         ; Make sure the new overlay overrides mark-mode's

  (setq region-cursor-overlays               ; Append the new overlay to the list of current overlays
        (cons overlay
              region-cursor-overlays)))



;; Moves an overlay to the current point
(defun move-region-cursor-overlay (overlay)
  (setq position
        (point))                             ; Save the current point

  (move-overlay overlay
                (- position 1) position)     ; Move the overlay to the new position

  (setq region-cursor-overlays               ; Append the moved overlay to the list of current overlays
        (cons overlay
              region-cursor-overlays)))



;; Highlight the multi-line cursor of the current region
(defun region-cursor-highlight ()
  (when (region-active-p)                    ; Ensure we have a region

    (setq cur-col                            ; We want to try and put the cursor in the column to the right
          (+ 1 (pos-col (point)))            ; of the current column. This makes sure that there's a character
                                             ; where we draw the overlay

          unreused-overlays                  ; Make a list of the overlays we can still reuse
          region-cursor-overlays

          region-cursor-overlays             ; Reset the list of current overlays
          nil)

    (region-apply                            ; For every line in the multi-line cursor
     (lambda ()
       (move-to-column cur-col)

       (when (eq (pos-col (point))           ; Before we draw an overlay, make sure there's a character
                 cur-col)                    ; to draw over

         (if (eq nil unreused-overlays)      ; If there are no more reusable overlays,
             (new-region-cursor-overlay)     ; make a new one

           (move-region-cursor-overlay       ; Otherwise, move the next reusable overlay,
            (car unreused-overlays))

           (setq unreused-overlays           ; and remove it from the list of remaining reusable
                 (cdr unreused-overlays)))))); overlays

    (dolist (overlay unreused-overlays)      ; For every remaining reusable overlay
      (delete-overlay overlay))              ; Kill it

    (setq deactivate-mark nil)))             ; Don't deactivate the active region





(rectangle-mark-mode t)

(define-key rectangle-mark-mode-map (kbd "ESC <up>")    'region-shift-up)
(define-key rectangle-mark-mode-map (kbd "ESC <left>")  'region-shift-left)
(define-key rectangle-mark-mode-map (kbd "ESC <right>") 'region-shift-right)
(define-key rectangle-mark-mode-map (kbd "ESC <next>")  'region-snap-right)



(add-hook 'activate-mark-hook
          (lambda ()
            (add-hook
               'post-self-insert-hook
               'region-insert)

            (add-hook
             'post-command-hook
             'region-stats)

            (add-hook
             'post-command-hook
             'region-cursor-highlight)

            (define-key (current-global-map) [remap backward-delete-char-untabify] 'region-backspace)))



(add-hook 'deactivate-mark-hook
          (lambda ()
            (remove-hook
               'post-self-insert-hook
               'region-insert)

            (remove-hook
             'post-command-hook
             'region-stats)

            (remove-hook
             'post-command-hook
             'region-cursor-highlight)

            (kill-region-cursor-overlays)

            (define-key (current-global-map) [remap backward-delete-char-untabify] nil)))



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