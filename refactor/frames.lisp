(defpackage :stumpwm-ng
  (:use :cl))

(in-package :stumpwm-ng)

(defclass group ()
  ((screen :initarg :screen :accessor group-screen)
   (windows :initform nil :accessor group-windows)
   (current-window :initform nil :accessor group-current-window)
   (number :initarg :number :accessor group-number)
   (name :initarg :name :accessor group-name)
   (on-top-windows :initform nil :accessor group-on-top-windows)))

(defclass tile-group (group)
  ((frame-tree :accessor tile-group-frame-tree)
   (last-frame :initform nil :accessor tile-group-last-frame)
   (current-frame :accessor tile-group-current-frame)))

(frame-tree)

(defclass frame () ())

(defstruct frame
  (number nil :type integer)
  x
  y
  width
  height
  window)

(defstruct (head (:include frame)))

(defclass screen ()
  ((id :initarg :id :reader screen-id)
   (host :initarg :host :reader screen-host)
   (number :initarg :number :reader screen-number)
   (heads :initform () :accessor screen-heads)
   (groups :initform () :accessor screen-groups)
   (current-group :accessor screen-current-group)
   ;; various colors (as returned by alloc-color)
   (border-color :initarg :border-color :accessor screen-border-color)
   (fg-color :initarg :fg-color :accessor screen-fg-color)
   (bg-color :initarg :bg-color :accessor screen-bg-color)
   (win-bg-color :initarg :win-bg-color :accessor screen-win-bg-color)
   (focus-color :initarg :focus-color :accessor screen-focus-color)
   (unfocus-color :initarg :unfocus-color :accessor screen-unfocus-color)
   (float-focus-color :initarg :float-focus-color :accessor screen-float-focus-color)
   (float-unfocus-color :initarg :float-unfocus-color :accessor screen-float-unfocus-color)
   (msg-border-width :initarg :msg-border-width :accessor screen-msg-border-width)
   (frame-outline-width :initarg :frame-outline-width :accessor screen-frame-outline-width)
   (fonts :initarg :fonts :accessor screen-fonts)
   (mapped-windows :initform () :accessor screen-mapped-windows :documentation
    "A list of all mapped windows. These are the raw xlib:window's. window structures are stored in groups.")
   (withdrawn-windows :initform () :accessor screen-withdrawn-windows :documentation
    "A list of withdrawn windows. These are of type stumpwm::window
and when they're mapped again they'll be put back in the group
they were in when they were unmapped unless that group doesn't
exist, in which case they go into the current group.")
   (urgent-windows :initform () :accessor screen-urgent-windows :documentation
    "a list of windows for which (window-urgent-p) currently true.")
   (input-window :initarg :input-window :reader screen-input-window)
   (key-window :initarg :key-window :reader screen-key-window :documentation
    "the window that accepts further keypresses after a toplevel key has been pressed.")
   (focus-window :initarg :focus-window :reader screen-focus-window :documentation
    "The window that gets focus when no window has focus")
   (frame-window :initarg :frame-window :reader screen-frame-window)
   (frame-outline-gc :initarg :frame-outline-gc :reader screen-frame-outline-gc)
   ;; color contexts
   (message-cc :initarg :message-cc :reader screen-message-cc)
   ;; color maps
   (color-map-normal :initform nil :accessor screen-color-map-normal)
   (color-map-bright :initform nil :accessor screen-color-map-bright)
   (ignore-msg-expose :initform 0 :accessor screen-ignore-msg-expose :documentation
    "used to ignore the first expose even when mapping the message window.")
   ;; the window that has focus
   (focus :initform nil :accessor screen-focus)
   (current-msg :initform nil :accessor screen-current-msg)
   (current-msg-highlights :initform nil :accessor screen-current-msg-highlights)
   (last-msg :initform nil :accessor screen-last-msg)
   (last-msg-highlights :initform nil :accessor screen-last-msg-highlights)))
