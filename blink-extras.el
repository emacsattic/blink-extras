
;;;; -*-Emacs-Lisp-*- Enhancements to Lucid GNU Emacs' "blink-paren" Package
;;;; Written by Eric Eide, last modified on $Date: 1994/07/27 16:55:55 $.
;;;; (C) Copyright 1994, Eric Eide and the University of Utah
;;;;
;;;; COPYRIGHT NOTICE
;;;;
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation; either version 2 of the License, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;; for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;;;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;;; AUTHOR
;;;;
;;;; This set of functions was written by Eric Eide (eeide@cs.utah.edu).  All
;;;; of these functions, however, are simply minor variations of functions from
;;;; Lucid GNU Emacs' "blink-paren" package.
;;;;
;;;; Address:
;;;;
;;;;   Eric Eide (eeide@cs.utah.edu)
;;;;   University of Utah
;;;;   3190 Merrill Engineering Building
;;;;   Salt Lake City, Utah  84112
;;;;
;;;; Lucid GNU Emacs and its "blink-paren" package are distributed under the
;;;; terms of the GNU General Public License.

;;;; LISP CODE DIRECTORY INFORMATION
;;;;
;;;; LCD Archive Entry:
;;;; blink-extras|Eric Eide|eeide@cs.utah.edu|
;;;; Enhancements to Lucid GNU Emacs' "blink-paren" package|
;;;; $Date: 1994/07/27 16:55:55 $|$Revision: 1.2 $||

;;;; SUMMARY
;;;;
;;;; This file redefines several functions from Lucid GNU Emacs' "blink-paren"
;;;; package.  In fact, this file redefines *most* of the functions from that
;;;; package!  The changes are described below.
;;;;
;;;;   + `blink-paren-make-extent' has been changed so that when point is
;;;;	 immediately after a quoted list form --- '(foo), for example --- the
;;;;	 blinking extent will be located on the opening parenthesis and not on
;;;;	 the quote.
;;;;
;;;;   + `blink-paren-make-extent' has also been changed so that when the
;;;;	 matching parenthesis is not visible in the window, no blinking extent
;;;;	 is created.  This saves CPU time because the interval timer won't be
;;;;	 created either.
;;;;
;;;;   + Finally, `blink-paren-make-extent' now displays an appropriate message
;;;;	 ("Matches ...") in the echo area when the matching parenthesis is not
;;;;	 visible in the window.  This provides valuable feedback.  This feature
;;;;	 is controlled by the new user option `blink-paren-messages'.
;;;;
;;;;   + `blink-paren-timeout' and `blink-paren-post-command' have been changed
;;;;	 to keep track of the number of times that a parenthesis has blinked.
;;;;	 A new user option, `blink-paren-times', determines the number of times
;;;;	 that a parenthesis will blink on and off without any intervening user
;;;;	 commands.  This means that if the user is idle, the blinking will
;;;;	 eventually stop.  (One can, however, set `blink-paren-times' to `nil'
;;;;	 to disable this feature.)
;;;;
;;;;   + `blink-paren-post-command' has also been changed not to activate the
;;;;     blinking if user input is already available.
;;;;
;;;;   + `blink-paren' has been changed to leave the variable `blink-matching-
;;;;	 paren' alone.
;;;;
;;;; In addition, this file adds `blink-paren-post-command' to the `select-
;;;; screen-hook' and `blink-paren-pre-command' to the `deselect-screen-hook'.
;;;; This means that a blinking parenthesis will stop blinking when its screen
;;;; is deselected (thereby reducing the CPU time consumed by the "idle" Emacs
;;;; process) and then start again when the screen is (re)selected.
;;;;
;;;; Because `blink-matching-paren' is set to `nil' when the "blink-paren"
;;;; package is loaded, this file of resets that variable to its default value
;;;; (`t').
;;;;
;;;; To use this file, simply load it after the "blink-paren" package has been
;;;; loaded.  Alternately, because "blink-extras" will automatically load the
;;;; "blink-paren" package as necessary, one can simply load "blink-extras"
;;;; instead of "blink-paren".

;; Note that until Lucid 19.10, "blink-paren" didn't provide `blink-paren'.
(require 'blink-paren)

;; (provide 'blink-extras) at the end of this file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the global variable declarations.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar blink-paren-times (if (and (numberp blink-paren-timeout)
				   (> blink-paren-timeout 0))
			      ;; Default: blink for about 30 seconds.
			      (floor 30 blink-paren-timeout)
			    nil)
  "*The maximum number of times that a parenthesis will blink when the user is
idle.  If this is `nil' then parentheses will blink indefinitely.")

(defvar blink-paren-count 0
  "The counter used internally by `blink-paren-timeout' to count the number of
blinks.")

(defvar blink-paren-messages t
  "*When this flag is set to a non-`nil' value, informative messages will be
displayed in the echo area when the matching parenthesis is not visible in the
selected window.  This can provide valuable feedback when editing large forms."
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the new function definitions.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; The new version of `blink-paren-make-extent' that works better for quoted
;;; list forms.  It also does special things when the matching parenthesis is
;;; not visible in the window.
;;;

(defun blink-paren-make-extent ()
  ;; Make an extent on the matching parenthesis, if any.  Return it.
  (let ((dir (blink-paren-sexp-dir)))
    (and dir
	 (condition-case ()
	     (let* ((parse-sexp-ignore-comments t)
		    (other-pos (scan-sexps (point) dir))
		    (other-pos-visible (and other-pos
					    (pos-visible-in-window-p other-pos)
					    )))
	       ;; If the matching paren is not visible in the window, display
	       ;; an appropriate message.
	       (if (and other-pos
			(not other-pos-visible)
			blink-paren-messages
			;; Do not overwrite an existing message, however.
			(let ((current-message (message-displayed-p t)))
			  (or (not current-message)
			      (= 0 (length current-message))
			      (save-match-data
				(string-match "\\`\\s-*\\'" current-message))
			      )))
		   (blink-paren-message (if (= dir 1)
					    (1- other-pos)
					  other-pos)))
	       ;; Construct and return the blinking paren extent, if
	       ;; appropriate.
	       (and other-pos
		    (or other-pos-visible
			highlight-paren-expression)
		    (let ((extent
			   (if (= dir 1)
			       (make-extent (if highlight-paren-expression
						(point)
					      (- other-pos 1))
					    other-pos)
			     (make-extent other-pos
					  (if highlight-paren-expression
					      (point)
					    (+ other-pos 1))))))
		      (set-extent-face extent (if highlight-paren-expression
						  'highlight-expression
						'blink-paren-on))
		      extent))
	       )
	   (error nil))
	 )))

;;;
;;; This is a new auxiliary function for `blink-paren-make-extent', above.
;;;

(defun blink-paren-message (match-pos)
  "Display a message that describes where the parenthesis at MATCH-POS is.
This function is called by `blink-paren-make-extent' when MATCH-POS is not
visible in the selected window."
  ;; This logic was stolen almost verbatim from `blink-matching-open' in the
  ;; Lucid GNU Emacs file "simple.el".  The messages could certainly be
  ;; improved.  Often, they are confusing and don't provide very much context.
  (save-excursion
    (goto-char match-pos)
    (message (gettext "Matches %s")
	     (cond ;; Show what precedes the `match-pos' in its line, if
		   ;; anything.
		   ((save-excursion
		      (skip-chars-backward " \t")
		      (not (bolp)))
		    (buffer-substring (progn (beginning-of-line) (point))
				      (1+ match-pos)))
		   
		   ;; Show what follows the `match-pos' in its line, if
		   ;; anything.
		   ((save-excursion
		      (forward-char 1)
		      (skip-chars-forward " \t")
		      (not (eolp)))
		    (buffer-substring match-pos
				      (progn (end-of-line) (point))))
		   
		   ;; Otherwise show the previous nonblank line.
		   (t
		    (concat
		     (buffer-substring (progn
					 (backward-char 1)
					 (skip-chars-backward "\n \t")
					 (beginning-of-line)
					 (point))
				       (progn (end-of-line)
					      (skip-chars-backward " \t")
					      (point)))
		     ;; Replace the newline and other whitespace with `...'.
		     "..."
		     (buffer-substring match-pos (1+ match-pos))))
		   ))
    ))

;;;
;;; The new versions of `blink-paren-timeout' and `blink-paren-post-command'
;;; that count the number of times that a parenthesis has blinked.
;;;

(defun blink-paren-timeout (arg)
  ;; This is the callback for the `blink-paren-timeout'.  It changes the face
  ;; of the blinking parenthesis.
  ;;
  ;; The extent could have been deleted for some reason and not point to a
  ;; buffer anymore.  So catch any error; if one occurs, remove the timeout.
  (condition-case nil
      (let ((face (extent-face blink-paren-extent)))
	(set-extent-face blink-paren-extent (if (eq face 'blink-paren-on)
						'blink-paren-off
					      'blink-paren-on))
	(if (and (integerp blink-paren-times)
		 (eq face 'blink-paren-off)
		 (>= (setq blink-paren-count (1+ blink-paren-count))
		     blink-paren-times))
	    ;; We've timed out.  Turn the blinking off.
	    (blink-paren-pre-command))
	)
    (error (blink-paren-pre-command))))

(defun blink-paren-post-command ()
  ;; This function is called as part of the `post-command-hook' after each
  ;; command is executed.  Insert the extent and add the timeout if point is on
  ;; a parenthesis.
  (blink-paren-pre-command)
  (if (and (or (not (integerp blink-paren-times))
	       (> blink-paren-times 0))
	   (not (input-pending-p))
	   (setq blink-paren-extent (blink-paren-make-extent))
	   (not highlight-paren-expression)
	   (not (and (face-equal 'blink-paren-on 'blink-paren-off)
		     (progn
		       (set-extent-face blink-paren-extent 'blink-paren-on)
		       t)))
	   (or (floatp blink-paren-timeout)
	       (integerp blink-paren-timeout)))
      (setq blink-paren-count 0
	    blink-paren-timeout-id (add-timeout blink-paren-timeout
						'blink-paren-timeout
						()
						blink-paren-timeout))
    ))

;;;
;;; The new version of `blink-paren' that does not modify the variable
;;; `blink-matching-paren'.
;;;

(defun blink-paren (&optional arg)
  "Toggles paren blinking on and off.
With a positive argument, turns it on.
With a non-positive argument, turns it off."
  (interactive "P")
  (let* ((was-on (not (not (memq 'blink-paren-pre-command pre-command-hook))))
	 (on-p (if (null arg)
		   (not was-on)
		(> (prefix-numeric-value arg) 0))))
    (cond ((eq on-p was-on)
	   nil)
	  (on-p
	   (add-hook 'pre-command-hook 'blink-paren-pre-command)
	   (add-hook 'post-command-hook 'blink-paren-post-command)
	   ;; (setq blink-matching-paren nil)
	   )
	  (t
	   (remove-hook 'pre-command-hook 'blink-paren-pre-command)
	   (remove-hook 'post-command-hook 'blink-paren-post-command)
	   (and blink-paren-extent (detach-extent blink-paren-extent))
	   ;; (setq blink-matching-paren t)
	   ))
    on-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are some additional tweaks.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; When a screen is deselected, turn off any blinking parenthesis.  When a
;; screen is (re)selected, turn the blinking back on.
;;

(add-hook 'deselect-screen-hook (function blink-paren-pre-command))
(add-hook 'select-screen-hook   (function blink-paren-post-command))

;;
;; The loading of "blink-paren" set `blink-matching-paren' to `nil'.  Set it
;; back to its default value.
;;

(setq blink-matching-paren t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Finally, here is the `provide' statement.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'blink-extras)

;; End of file.

