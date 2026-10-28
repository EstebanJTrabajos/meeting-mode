;;; meeting-mode-tests.el --- Tests for meeting-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Esteban Trabajos

;; Author: Esteban Trabajos <etrabajos@bozoslivehe.re>
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

(ert-deftest meeting-mode-loads-without-config ()
  (should (featurep 'meeting-mode))
  (should (commandp 'meeting-mode))
  (should (custom-variable-p 'meeting-flash-duration))
  (should (custom-variable-p 'meeting-flash-alpha))
  (should (custom-variable-p 'meeting-accent-color)))

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
                      transient-setup-buffer-hook))
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
                      transient-setup-buffer-hook)))
    (meeting-mode -1)))

(provide 'meeting-mode-tests)
;;; meeting-mode-tests.el ends here
