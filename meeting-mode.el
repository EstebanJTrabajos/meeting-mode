;;; meeting-mode.el --- Highlight the focused window during screen sharing -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Esteban Trabajos

;; Author: Esteban Trabajos <etrabajos@bozoslivehe.re>
;; Maintainer: Esteban Trabajos <etrabajos@bozoslivehe.re>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, frames, faces
;; URL: https://github.com/estebanjtrabajos/meeting-mode

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `meeting-mode' makes the focused Emacs window easy to follow while
;; sharing a screen.  When focus changes, it briefly tints the newly focused
;; window and keeps a theme-derived accent on that window's fringe, mode line,
;; header line, and tab line.  It follows ordinary window selection, buffer
;; switches, the active minibuffer, and (when available) Transient menus.
;;
;; Enable it with M-x meeting-mode.  There is no default key binding:
;;
;;   (keymap-set global-map "C-c m" #'meeting-mode)
;;
;; The accent is derived from the current theme's `isearch', `highlight',
;; `region', or `mode-line' background.  Customize `meeting-accent-color' to
;; use a fixed color, or adjust `meeting-flash-duration' and
;; `meeting-flash-alpha' to change the flash.
;;
;; The implementation is inspired by `winpulse' (momentary window flashes)
;; and `selected-window-accent-mode' (focused-window fringes and mode line),
;; but it does not require either package.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'seq)
(require 'subr-x)

(defgroup meeting nil
  "Make the focused Emacs window easy to follow."
  :prefix "meeting-"
  :group 'convenience)

(defvar meeting-mode nil
  "Non-nil when `meeting-mode' is enabled.")

(defcustom meeting-flash-duration 0.15
  "Seconds to keep the focused-window tint visible."
  :type 'number
  :group 'meeting
  :package-version '(meeting-mode . "0.1.0"))

(defcustom meeting-flash-alpha 0.22
  "Strength of the focused-window tint, from 0 to 1."
  :type 'number
  :group 'meeting
  :package-version '(meeting-mode . "0.1.0"))

(defcustom meeting-accent-color nil
  "Optional fixed accent color.
When nil, derive the accent from the current theme."
  :type '(choice (const :tag "Derive from theme" nil)
                 (color :tag "Fixed color"))
  :group 'meeting
  :package-version '(meeting-mode . "0.1.0"))

(defface meeting-accent
  '((t :inherit highlight :extend t))
  "Accent face for focused-window bars and mode lines."
  :group 'meeting
  :package-version '(meeting-mode . "0.1.0"))

(defface meeting-accent-bg
  '((t :background "#e6a700" :extend t))
  "Background-only accent face for focused-window fringes."
  :group 'meeting
  :package-version '(meeting-mode . "0.1.0"))

(defvar meeting--accent "#e6a700"
  "Current accent color.")

(defvar-local meeting--face-cookies nil
  "Face-remapping cookies installed in this buffer.")

(defvar meeting--flash-timer nil)
(defvar meeting--flash-window nil)
(defvar meeting--flash-buffer nil)
(defvar meeting--flash-cookie nil)
(defvar meeting--refresh-timer nil)
(defvar meeting--refresh-pending-flash nil)
(defvar meeting--last-window nil)
(defvar meeting--last-buffer nil)

(defconst meeting--accent-faces
  '((fringe . meeting-accent-bg)
    (mode-line . meeting-accent)
    (mode-line-active . meeting-accent)
    (header-line . meeting-accent)
    (tab-line . meeting-accent))
  "Faces remapped only in the focused window.")

(defun meeting--valid-color-p (color)
  "Return non-nil when COLOR is a valid color string."
  (and (stringp color)
       (ignore-errors (color-name-to-rgb color))
       t))

(defun meeting--theme-accent ()
  "Return the best theme-derived accent color."
  (or (and (meeting--valid-color-p meeting-accent-color)
           meeting-accent-color)
      (let ((default-bg (face-background 'default nil t)))
        (or (seq-some
             (lambda (face)
               (let ((color (face-attribute face :background nil t)))
                 (and (meeting--valid-color-p color)
                      (not (and (meeting--valid-color-p default-bg)
                                (equal (downcase color)
                                       (downcase default-bg))))
                      color)))
             '(isearch highlight region mode-line))
            "#e6a700"))))

(defun meeting--contrast-color (color)
  "Return black or white for readable text over COLOR."
  (let ((rgb (ignore-errors (color-name-to-rgb color))))
    (if (and rgb
             (> (+ (* 0.299 (nth 0 rgb))
                   (* 0.587 (nth 1 rgb))
                   (* 0.114 (nth 2 rgb)))
                0.55))
        "#000000"
      "#ffffff")))

(defun meeting--blend (foreground background alpha)
  "Blend FOREGROUND into BACKGROUND by ALPHA."
  (let ((fg (ignore-errors (color-name-to-rgb foreground)))
        (bg (ignore-errors (color-name-to-rgb background))))
    (if (and fg bg)
        (apply #'color-rgb-to-hex
               (cl-mapcar (lambda (a b)
                            (+ (* alpha a) (* (- 1 alpha) b)))
                          fg bg))
      (or foreground background))))

(defun meeting--regenerate-accent (&rest _)
  "Regenerate the meeting accent from the current theme."
  (let* ((accent (meeting--theme-accent))
         (foreground (meeting--contrast-color accent)))
    (setq meeting--accent accent)
    (set-face-attribute 'meeting-accent nil
                        :background accent
                        :foreground foreground
                        :extend t)
    (set-face-attribute 'meeting-accent-bg nil
                        :background accent
                        :foreground 'unspecified
                        :extend t)
    (when meeting-mode
      (dolist (frame (frame-list))
        (force-window-update frame)))))

(defun meeting--ensure-remaps (&optional buffer)
  "Install meeting accent remaps in BUFFER, once."
  (with-current-buffer (or buffer (current-buffer))
    (unless meeting--face-cookies
      (setq-local meeting--face-cookies
                  (delq nil
                        (mapcar
                         (lambda (entry)
                           (when (facep (car entry))
                             (face-remap-add-relative
                              (car entry)
                              `(:filtered (:window meeting--focused t)
                                          ,(cdr entry)))))
                         meeting--accent-faces))))))

(defun meeting--remove-remaps (&optional buffer)
  "Remove meeting accent remaps from BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (when meeting--face-cookies
      (mapc #'face-remap-remove-relative meeting--face-cookies)
      (setq meeting--face-cookies nil))))

(defun meeting--target-window ()
  "Return the window that currently owns keyboard focus."
  (or (active-minibuffer-window)
      (selected-window)))

(defun meeting--set-focused-window (window)
  "Mark WINDOW as focused and refresh all meeting accents."
  (dolist (frame (frame-list))
    (dolist (win (window-list frame t))
      (set-window-parameter win 'meeting--focused (eq win window))))
  (when (window-live-p window)
    (meeting--ensure-remaps (window-buffer window)))
  (dolist (frame (frame-list))
    (force-window-update frame)))

(defun meeting--clear-flash ()
  "Remove the current focused-window flash."
  (when (timerp meeting--flash-timer)
    (cancel-timer meeting--flash-timer))
  (when (and meeting--flash-buffer
             (buffer-live-p meeting--flash-buffer)
             meeting--flash-cookie)
    (with-current-buffer meeting--flash-buffer
      (face-remap-remove-relative meeting--flash-cookie)))
  (when (and meeting--flash-window
             (window-live-p meeting--flash-window))
    (set-window-parameter meeting--flash-window 'meeting--flash nil))
  (setq meeting--flash-timer nil
        meeting--flash-window nil
        meeting--flash-buffer nil
        meeting--flash-cookie nil))

(defun meeting--flash-window (window)
  "Briefly tint WINDOW to announce focus."
  (when (and meeting-mode (window-live-p window))
    (meeting--clear-flash)
    (let* ((buffer (window-buffer window))
           (background (or (face-background 'default nil t) "#ffffff"))
           (color (meeting--blend meeting--accent background
                                     meeting-flash-alpha)))
      (meeting--ensure-remaps buffer)
      (setq meeting--flash-window window
            meeting--flash-buffer buffer)
      (with-current-buffer buffer
        (setq meeting--flash-cookie
              (face-remap-add-relative
               'default
               `(:filtered (:window meeting--flash t)
                           (:background ,color)))))
      (set-window-parameter window 'meeting--flash t)
      (force-window-update window)
      (setq meeting--flash-timer
            (run-at-time meeting-flash-duration nil
                         #'meeting--clear-flash)))))

(defun meeting--refresh (&optional flash)
  "Update the meeting accent, and flash when FLASH is non-nil."
  (when meeting-mode
    (let* ((window (meeting--target-window))
           (buffer (and (window-live-p window) (window-buffer window))))
      (when (window-live-p window)
        (meeting--set-focused-window window)
        (when (and flash
                   (or (not (eq window meeting--last-window))
                       (not (eq buffer meeting--last-buffer))))
          (meeting--flash-window window))
        (setq meeting--last-window window
              meeting--last-buffer buffer)))))

(defun meeting--schedule-refresh (&optional flash delay)
  "Schedule a meeting refresh, preserving any pending FLASH request.
DELAY defaults to 20 milliseconds."
  (when meeting-mode
    (setq meeting--refresh-pending-flash
          (or meeting--refresh-pending-flash flash))
    (when (timerp meeting--refresh-timer)
      (cancel-timer meeting--refresh-timer))
    (setq meeting--refresh-timer
          (run-at-time (or delay 0.02) nil
                       (lambda ()
                         (setq meeting--refresh-timer nil)
                         (let ((pending meeting--refresh-pending-flash))
                           (setq meeting--refresh-pending-flash nil)
                           (meeting--refresh pending)))))))

(defun meeting--on-window-selection-change (&rest _)
  "Handle a window selection change."
  (meeting--schedule-refresh t))

(defun meeting--on-window-state-change (&rest _)
  "Handle window geometry or buffer changes."
  (meeting--schedule-refresh nil))

(defun meeting--on-window-buffer-change (&rest _)
  "Handle a buffer change in a window."
  (meeting--schedule-refresh t))

(defun meeting--on-minibuffer-setup ()
  "Handle entering the minibuffer, including Vertico completion."
  (when (active-minibuffer-window)
    (meeting--ensure-remaps
     (window-buffer (active-minibuffer-window))))
  (meeting--schedule-refresh t 0.01))

(defun meeting--on-minibuffer-exit ()
  "Restore the accent after leaving the minibuffer."
  (meeting--schedule-refresh nil 0.01))

(defun meeting--focus-transient ()
  "Focus and flash the transient popup when it is visible."
  (when meeting-mode
    (let* ((transient-window (and (boundp 'transient--window)
                                  (symbol-value 'transient--window)))
           (window (or (and (window-live-p transient-window)
                            transient-window)
                       (get-buffer-window " *transient*" t)
                       (selected-window))))
      (when (window-live-p window)
        (meeting--set-focused-window window)
        (when (or (not (eq window meeting--last-window))
                  (not (eq (window-buffer window)
                           meeting--last-buffer)))
          (meeting--flash-window window)
          (setq meeting--last-window window
                meeting--last-buffer (window-buffer window)))))))

(defun meeting--on-transient-setup ()
  "Handle a transient menu becoming visible."
  (when meeting-mode
    (run-at-time 0.03 nil #'meeting--focus-transient)))

(defun meeting--on-transient-exit ()
  "Restore the accent after a transient menu closes."
  (meeting--schedule-refresh nil 0.03))

(defun meeting--on-frame-focus (&rest _)
  "Handle focus moving back into Emacs."
  (when (and meeting-mode (frame-focus-state))
    (meeting--schedule-refresh t 0.05)))

;;;###autoload
(define-minor-mode meeting-mode
  "Make the focused Emacs window easy to follow during screen sharing.
The focused window briefly flashes when focus moves and keeps a thin
theme-derived accent on its fringes, mode line, and header line."
  :global t
  :lighter " MTG"
  :group 'meeting
  (if meeting-mode
      (progn
        (meeting--regenerate-accent)
        (add-hook 'window-selection-change-functions
                  #'meeting--on-window-selection-change)
        (add-hook 'window-state-change-functions
                  #'meeting--on-window-state-change)
        (add-hook 'window-buffer-change-functions
                  #'meeting--on-window-buffer-change)
        (add-hook 'minibuffer-setup-hook #'meeting--on-minibuffer-setup)
        (add-hook 'minibuffer-exit-hook #'meeting--on-minibuffer-exit)
        (add-hook 'transient-setup-buffer-hook
                  #'meeting--on-transient-setup)
        (add-hook 'transient-exit-hook #'meeting--on-transient-exit)
        (add-hook 'enable-theme-functions #'meeting--regenerate-accent)
        (add-hook 'disable-theme-functions #'meeting--regenerate-accent)
        (add-function :after after-focus-change-function
                      #'meeting--on-frame-focus)
        (meeting--refresh))
    (remove-hook 'window-selection-change-functions
                 #'meeting--on-window-selection-change)
    (remove-hook 'window-state-change-functions
                 #'meeting--on-window-state-change)
    (remove-hook 'window-buffer-change-functions
                 #'meeting--on-window-buffer-change)
    (remove-hook 'minibuffer-setup-hook #'meeting--on-minibuffer-setup)
    (remove-hook 'minibuffer-exit-hook #'meeting--on-minibuffer-exit)
    (remove-hook 'transient-setup-buffer-hook #'meeting--on-transient-setup)
    (remove-hook 'transient-exit-hook #'meeting--on-transient-exit)
    (remove-hook 'enable-theme-functions #'meeting--regenerate-accent)
    (remove-hook 'disable-theme-functions #'meeting--regenerate-accent)
    (remove-function after-focus-change-function #'meeting--on-frame-focus)
    (when (timerp meeting--refresh-timer)
      (cancel-timer meeting--refresh-timer))
    (setq meeting--refresh-timer nil
          meeting--refresh-pending-flash nil
          meeting--last-window nil
          meeting--last-buffer nil)
    (meeting--clear-flash)
    (dolist (frame (frame-list))
      (dolist (win (window-list frame t))
        (set-window-parameter win 'meeting--focused nil)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (meeting--remove-remaps buffer)))))

(defun meeting-mode-unload-function ()
  "Disable `meeting-mode' before unloading the package."
  (when meeting-mode
    (meeting-mode -1))
  nil)

(provide 'meeting-mode)
;;; meeting-mode.el ends here
