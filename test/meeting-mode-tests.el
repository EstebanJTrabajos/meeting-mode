;;; meeting-mode-tests.el --- Tests for meeting-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Esteban Trabajos

;; Author: Esteban Trabajos <etrabajos@bozoslivehe.re>
;; Assisted-by: OpenCode:deepseek-v4.1-flash-1017
;; URL: https://github.com/estebanjtrabajos/meeting-mode
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for meeting-mode.  These tests intentionally load the package
;; without any user configuration and without requiring Transient.

;;; Code:

(require 'ert)
(require 'meeting-mode)

;; Declared here so the Transient tests can bind them without loading
;; Transient itself.  These are the variables declared by `transient'.
(defvar transient--buffer)
(defvar transient--showp)

(defun meeting-mode-test--hook-value (hook)
  "Return HOOK's value when HOOK is bound, otherwise nil."
  (and (boundp hook) (symbol-value hook)))

(ert-deftest meeting-mode-loads-without-config ()
  (should (featurep 'meeting-mode))
  (should (commandp 'meeting-mode))
  (should (custom-variable-p 'meeting-flash-duration))
  (should (custom-variable-p 'meeting-flash-alpha))
  (should (custom-variable-p 'meeting-accent-color))
  (should (custom-variable-p 'meeting-border-width))
  (should (custom-variable-p 'meeting-blink-count))
  (should (custom-variable-p 'meeting-blink-interval)))

(ert-deftest meeting-mode-has-no-default-global-binding ()
  (should-not (where-is-internal 'meeting-mode (current-global-map) t)))

(ert-deftest meeting-mode-enables-and-disables ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (should meeting-mode)
        (should (memq #'meeting--on-window-selection-change
                      window-selection-change-functions))
        (should (memq #'meeting--on-window-state-change
                      window-state-change-functions))
        (should (memq #'meeting--on-window-buffer-change
                      window-buffer-change-functions))
        (should (memq #'meeting--on-minibuffer-setup
                      minibuffer-setup-hook))
        (should (memq #'meeting--on-transient-setup
                      (meeting-mode-test--hook-value
                       'transient-setup-buffer-hook)))
        (should (memq #'meeting--regenerate-accent
                      enable-theme-functions))
        (should (window-parameter (selected-window)
                                  'meeting--focused))
        (with-current-buffer (window-buffer (selected-window))
          (should meeting--face-cookies))
        (should (stringp meeting--accent))
        (should (stringp (face-attribute 'meeting-accent :background)))
        (should (stringp (face-attribute 'meeting-accent :foreground))))
    (meeting-mode -1))
  (should-not meeting-mode)
  (should-not (window-parameter (selected-window) 'meeting--focused))
  (should-not (memq #'meeting--on-window-selection-change
                    window-selection-change-functions))
  (should-not (memq #'meeting--on-minibuffer-setup
                    minibuffer-setup-hook))
  (with-current-buffer (window-buffer (selected-window))
    (should-not meeting--face-cookies)))

(ert-deftest meeting-mode-accent-is-regenerated ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (setq meeting--accent "#000000")
        (meeting--regenerate-accent)
        (should (stringp meeting--accent))
        (should (equal (face-attribute 'meeting-accent :background)
                       meeting--accent))
        (should (stringp (face-attribute 'meeting-accent :foreground))))
    (meeting-mode -1)))

(ert-deftest meeting-mode-focus-parameter-follows-selected-window ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (let* ((original (selected-window))
               (other (split-window original)))
          (unwind-protect
              (progn
                (select-window other)
                (meeting--refresh)
                (should (window-parameter other 'meeting--focused))
                (should-not (window-parameter original 'meeting--focused))
                (select-window original)
                (meeting--refresh)
                (should (window-parameter original 'meeting--focused))
                (should-not (window-parameter other 'meeting--focused)))
            (when (window-live-p other)
              (delete-window other))
            (select-window original))))
    (meeting-mode -1)))

(ert-deftest meeting-mode-flash-is-transient ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (let ((window (selected-window))
              calls)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay repeat function &rest args)
                       (push (list delay repeat function args) calls)
                       'meeting-test-timer))
                    ((symbol-function 'cancel-timer) #'ignore))
            (meeting--flash-window window))
          (should (window-parameter window 'meeting--flash))
          (should calls)
          (with-current-buffer (window-buffer window)
            (should meeting--flash-cookie))
          (meeting--clear-flash)
          (should-not (window-parameter window 'meeting--flash))
          (should-not meeting--flash-cookie)))
    (meeting-mode -1)))

(ert-deftest meeting-mode-flash-blinks-the-configured-number ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (let ((window (selected-window))
              (meeting-blink-count 2)
              (meeting-blink-interval 150)
              callbacks)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest _args)
                       (push function callbacks)
                       'meeting-test-timer))
                    ((symbol-function 'cancel-timer) #'ignore))
            (meeting--flash-window window)
            (should (window-parameter window 'meeting--flash))
            (should (= meeting--flash-blinks-left 2))
            ;; First off phase.
            (funcall (pop callbacks))
            (should-not (window-parameter window 'meeting--flash))
            (should (= meeting--flash-blinks-left 1))
            ;; Second on phase.
            (funcall (pop callbacks))
            (should (window-parameter window 'meeting--flash))
            (should (= meeting--flash-blinks-left 1))
            ;; Final off phase.
            (funcall (pop callbacks))
            (should-not (window-parameter window 'meeting--flash))
            (should (= meeting--flash-blinks-left 0))
            (should-not meeting--flash-cookie))))
    (meeting-mode -1)))

(ert-deftest meeting-mode-blink-timing-defaults ()
  (let ((meeting-flash-duration 0.3)
        (meeting-blink-count 2)
        (meeting-blink-interval nil))
    (should (= (meeting--blink-interval-ms) 150))
    (should (= (meeting--flash-total-duration) 0.3)))
  (let ((meeting-blink-interval 200)
        (meeting-blink-count 3))
    (should (= (meeting--blink-interval-ms) 200))
    (should (= (meeting--flash-total-duration) 0.6))))

(ert-deftest meeting-mode-border-width-is-applied-and-restored ()
  (let ((original fringe-mode))
    (unwind-protect
        (progn
          (setq meeting-border-width 4)
          (meeting-mode 1)
          (should (equal fringe-mode (cons 4 4)))
          (meeting-mode -1)
          (should (equal fringe-mode original)))
      (meeting-mode -1)
      (setq fringe-mode original))))

(ert-deftest meeting-mode-scheduler-preserves-pending-flash ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _) 'meeting-test-timer))
                  ((symbol-function 'cancel-timer) #'ignore))
          (meeting--schedule-refresh t)
          (meeting--schedule-refresh nil)
          (should meeting--refresh-pending-flash)))
    (meeting-mode -1)))

(ert-deftest meeting-mode-transient-integration-is-optional ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (should (memq #'meeting--on-transient-setup
                      (meeting-mode-test--hook-value
                       'transient-setup-buffer-hook))))
    (meeting-mode -1)))

(ert-deftest meeting-mode-accent-faces-cover-inactive-display-areas ()
  (should (assq 'mode-line-inactive meeting--accent-faces))
  (when (facep 'header-line-inactive)
    (should (assq 'header-line-inactive meeting--accent-faces)))
  (when (facep 'tab-line-inactive)
    (should (assq 'tab-line-inactive meeting--accent-faces))))

(ert-deftest meeting-mode-transient-focus-prefers-popup ()
  (let* ((selected (selected-window))
         (popup (split-window selected))
         (buffer (get-buffer-create " *meeting-transient-test*")))
    (unwind-protect
        (progn
          (set-window-buffer popup buffer)
          (select-window selected)
          (let ((transient--showp t)
                (transient--buffer buffer))
            (should (eq (meeting--transient-window) popup))
            (should (eq (meeting--target-window) popup))))
      (kill-buffer buffer)
      (when (window-live-p popup)
        (delete-window popup))
      (select-window selected))))

(ert-deftest meeting-mode-transient-popup-gets-accent-areas ()
  (let ((buffer (get-buffer-create " *meeting-transient-test*")))
    (unwind-protect
        (progn
          (meeting-mode 1)
          (with-current-buffer buffer
            (setq header-line-format nil
                  mode-line-format nil))
          (let ((transient--buffer buffer)
                (transient--showp t))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _) nil)))
              (meeting--on-transient-setup)))
          (with-current-buffer buffer
            (should (equal header-line-format " "))
            (should (equal mode-line-format " "))))
      (kill-buffer buffer)
      (meeting-mode -1))))

(ert-deftest meeting-mode-vertico-posframe-border-follows-accent ()
  (skip-unless (fboundp 'vertico-posframe--get-border-color))
  (unwind-protect
      (let ((meeting-accent-color "#123456"))
        (meeting-mode 1)
        (should (advice-member-p #'meeting--vertico-border-color
                                 'vertico-posframe--get-border-color))
        (should (equal (vertico-posframe--get-border-color) "#123456")))
    (meeting-mode -1)))

(ert-deftest meeting-mode-vertico-flash-is-safe-without-posframe ()
  (unwind-protect
      (progn
        (meeting-mode 1)
        (meeting--flash-vertico-posframe)
        (should-not meeting--vertico-flash-frame))
    (meeting-mode -1)))

;;; meeting-mode-tests.el ends here
