;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      load-prefer-newer t
  ;; package-native-compile    t
  ) ; native compile packages

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq use-dialog-box nil
  redisplay-dont-pause t
  inhibit-startup-screen t)

(setq load-prefer-newer noninteractive)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setenv "LC_CTYPE" "en_US.UTF-8")

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)
