;;; implementation of a floating style window management group

(in-package #:stumpwm)

;;; floating window

(defclass float-window (window)
  ((last-width :initform 0 :accessor float-window-last-width)
   (last-height :initform 0 :accessor float-window-last-height)
   (last-x :initform 0 :accessor float-window-last-x)
   (last-y :initform 0 :accessor float-window-last-y)))

(defmethod window-frame ((w float-window))
  nil)

(defmethod (setf window-frame) (frame (w float-window))
  frame)


(defvar *float-window-border* 1)
(defvar *float-window-title-height* 10)

;; some book keeping functions
(defmethod (setf window-x) :before (val (window float-window))
  (setf (float-window-last-x window) (window-x window)))

(defmethod (setf window-y) :before (val (window float-window))
  (setf (float-window-last-y window) (window-y window)))

(defmethod (setf window-width) :before (val (window float-window))
  (setf (float-window-last-width window) (window-width window)))

(defmethod (setf window-height) :before (val (window float-window))
  (setf (float-window-last-height window) (window-height window)))

(defun float-window-move-resize (win &key x y width height (border *float-window-border*))
  ;; x and y position the parent window while width, height resize the
  ;; xwin (meaning the parent will have a larger width).
  (with-accessors ((xwin window-xwin)
                   (parent window-parent))
      win
    (xlib:with-state (parent)
      (xlib:with-state (xwin)
        (when x
          (setf (xlib:drawable-x parent) x
                (window-x win) x))
        (when y
          (setf (xlib:drawable-y parent) y
                (window-y win) y))
        (when width
          (setf (xlib:drawable-width parent) (+ (xlib:drawable-x xwin) width border)
                (xlib:drawable-width xwin) width
                (window-width win) width))
        (when height
          (setf (xlib:drawable-height parent) (+ (xlib:drawable-y xwin) height border)
                (xlib:drawable-height xwin) height
                (window-height win) height))))))

(defmethod update-decoration ((window float-window))
  (let ((group (window-group window)))
    (setf (xlib:window-background (window-parent window))
          (if (eq (group-current-window group) window)
              (screen-float-focus-color (window-screen window))
              (screen-float-unfocus-color (window-screen window))))
    (xlib:clear-area (window-parent window))))

(defmethod window-sync ((window float-window) hint)
  (declare (ignore hint)))

(defmethod window-head ((window float-window))
  (let ((left (window-x window))
        (right (+ (window-x window) (window-width window)))
        (top (window-y window))
        (bottom (+ (window-y window) (window-height window)))
        (heads (screen-heads (group-screen (window-group window)))))
    (flet ((within-frame-p (y x head)
             (and (>= x (frame-x head))
                  (< x (+ (frame-x head) (frame-width head)))
                  (>= y (frame-y head))
                  (< y (+ (frame-y head) (frame-height head))))))
      (or (find-if (lambda (head)
                     (or (within-frame-p top left head)
                         (within-frame-p top right head)
                         (within-frame-p bottom left head)
                         (within-frame-p bottom right head)))
                   heads)
          ;; Didn't find any head, so give up and return the first one
          ;; in the list.
          (first heads)))))

(defmethod window-visible-p ((win float-window))
  (eql (window-state win) +normal-state+))

(defmethod (setf window-fullscreen) :after (val (window float-window))
  (with-accessors ((last-x float-window-last-x)
                   (last-y float-window-last-y)
                   (last-width float-window-last-width)
                   (last-height float-window-last-height)
                   (parent window-parent))
      window
    (if val
        (let ((head (window-head window)))
          (with-accessors ((x window-x)
                           (y window-y)
                           (width window-width)
                           (height window-height))
              window
            (format t "major on: ~a ~a ~a ~a~%" x y width height))
          (set-window-geometry window :x 0 :y 0)
          (float-window-move-resize window
                                    :x (frame-x head)
                                    :y (frame-y head)
                                    :width (frame-width head)
                                    :height (frame-height head)
                                    :border 0)
          (format t "loot after: ~a ~a ~a ~a~%" last-x last-y last-width last-height))
        (progn
          (format t "fullscreenage: ~a ~a ~a ~a~%" last-x last-y last-width last-height)
          ;; restore the position
          (set-window-geometry window :x *float-window-border* :y *float-window-title-height*)
          (float-window-move-resize window
                                    :x last-x
                                    :y last-y
                                    :width last-width
                                    :height last-height)))))

(defmethod really-raise-window ((window float-window))
  (raise-window window))

;;; floating group

(defclass float-group (group)
  ((current-window :accessor float-group-current-window)))

(defmethod group-startup ((group float-group)))

(flet ((add-float-window (group window)
         (change-class window 'float-window)
         (float-window-align window)
         (group-focus-window group window)))
  (defmethod group-add-window ((group float-group) window &key &allow-other-keys)
    (add-float-window group window))
  (defmethod group-add-window (group (window float-window) &key &allow-other-keys)
    (add-float-window group window)))

(defun %float-focus-next (group)
  (if (group-windows group)
      (group-focus-window group (first (group-windows group)))
      (no-focus group nil)))

(defcommand (float-focus-next floating-group) () ()
  (%float-focus-next (current-group)))

(defmethod group-delete-window ((group float-group) (window float-window))
  (declare (ignore window))
  (%float-focus-next group))

(defmethod group-wake-up ((group float-group))
  (%float-focus-next group))

(defmethod group-suspend ((group float-group)))

(defmethod group-current-head ((group float-group))
  (if-let ((current-window (group-current-window group)))
    (window-head current-window)
    (multiple-value-bind (x y)
        (xlib:global-pointer-position *display*)
      (find-head-by-position (group-screen group) x y))))

(defun float-window-align (window)
  (with-accessors ((parent window-parent)
                   (xwin window-xwin)
                   (width window-width)
                   (height window-height))
      window
    (set-window-geometry window :x *float-window-border* :y *float-window-title-height*)
    (xlib:with-state (parent)
      (setf (xlib:drawable-width parent) (+ width (* 2 *float-window-border*))
            (xlib:drawable-height parent) (+ height *float-window-title-height* *float-window-border*)
            (xlib:window-background parent) (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                                                              "Orange")))
    (xlib:clear-area (window-parent window))))

(defmethod group-resize-request ((group float-group) window width height)
  (float-window-move-resize window :width width :height height))

(defmethod group-move-request ((group float-group) window x y relative-to)
  (declare (ignore relative-to))
  (float-window-move-resize window :x x :y y))

(defmethod group-raise-request ((group float-group) window type)
  (declare (ignore type))
  (group-focus-window group window))

(defmethod group-lost-focus ((group float-group))
  (%float-focus-next group))

(defmethod group-indicate-focus ((group float-group))
  )

(defmethod group-focus-window ((group float-group) window)
  (focus-window window))

(defmethod group-root-exposure ((group float-group))
  )

(defmethod group-add-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-remove-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-resize-head ((group float-group) oh nh)
  (declare (ignore oh nh)))

(defmethod group-sync-all-heads ((group float-group))
  )

(defmethod group-sync-head ((group float-group) head)
  (declare (ignore head))
  )

(defvar *snap-border-ratio* 1/3)

(defun snap-coordinate (origin size mouse)
  "Return either 0 or SIZE (relative coordinates)"
  (if (zerop size)
      0
      (let ((ratio (/ (- mouse origin) size)))
        (cond
          ((>= ratio (- 1 *snap-border-ratio*))
           (values size
                   :max
                   (lambda (new-mouse)
                     (values origin (- new-mouse origin)))))

          ((<= ratio *snap-border-ratio*)
           (values 0
                   :min
                   (lambda (new-mouse)
                     (values new-mouse (+ size (- origin new-mouse))))))
          (t
           (values (floor size 2)
                   :middle
                   (lambda (new-mouse)
                     (declare (ignore new-mouse))
                     (values origin size))))))))

;; !!!! ratios problems

(defmethod group-button-press (group x y (window float-window) &aux (parent (window-parent window)))
  (let* ((screen (group-screen group))
         (screen-width (screen-width screen))
         (screen-height (screen-height screen))
         (initial-width (xlib:drawable-width parent))
         (initial-height (xlib:drawable-height parent)))
    (when (member *mouse-focus-policy* '(:click :sloppy))
      (group-focus-window group window))
    
    ;; When in border
    (multiple-value-bind (relx rely same-screen-p child state-mask)
        (xlib:query-pointer parent)
      (declare (ignore same-screen-p child))
      (let ((action (let ((keys (xlib:make-state-keys state-mask)))
                      (cond
                        ((find :button-3 keys) :resize)
                        ((find :button-1 keys) :move))))
            (par-x (xlib:drawable-x parent))
            (par-y (xlib:drawable-y parent)))
        (when (intersection (modifiers-super *modifiers*)
                            (xlib:make-state-keys state-mask))
          
          (multiple-value-bind (snap-x x-gravity x-resizer)
              (snap-coordinate par-x initial-width x)
            (multiple-value-bind (snap-y y-gravity y-resizer)
                (snap-coordinate par-y initial-height y)
              (when (and (eq x-gravity :middle)
                         (eq y-gravity :middle))
                ;; move with mouse 3 when pointing in center, but block on edges
                (setf action :move-clamp))

              ;; When resizing warp pointer to closest-corner
              (when (eq action :resize)
                (xlib:warp-pointer parent snap-x snap-y))

              (labels ((move-window-event-handler
                           (&rest event-slots &key event-key &allow-other-keys)
                         (case event-key
                           (:button-release :done)
                           (:motion-notify
                            (with-accessors ((parent window-parent))
                                window
                              (xlib:with-state (parent)
                                ;; Either move or resize the window
                                (case action
                                  ((:move :move-clamp)
                                   (let ((nx (- (getf event-slots :x) relx))
                                         (ny (- (getf event-slots :y) rely)))
                                     (when (eq action :move-clamp)
                                       (setf nx (clamp nx 0 (- screen-width initial-width)))
                                       (setf ny (clamp ny 0 (- screen-height initial-height))))
                                     (float-window-move-resize window :x nx :y ny)))
                                  (:resize
                                   (multiple-value-bind (nx nw) (funcall x-resizer (getf event-slots :x))
                                     (multiple-value-bind (ny nh) (funcall y-resizer (getf event-slots :y))
                                       (float-window-move-resize window
                                                                 :x nx
                                                                 :y ny
                                                                 :width  (max nw *min-frame-width*)
                                                                 :height (max nh *min-frame-height*))))))))
                            t)
                           ;; We need to eat these events or they'll ALL
                           ;; come blasting in later. Also things start
                           ;; lagging hard if we don't (on clisp anyway).
                           (:configure-notify t)
                           (:exposure t)
                           (t nil))))
                (xlib:grab-pointer (screen-root screen) '(:button-release :pointer-motion))
                (unwind-protect
                     ;; Wait until the mouse button is released
                     (loop for ev = (xlib:process-event *display*
                                                        :handler #'move-window-event-handler
                                                        :timeout nil
                                                        :discard-p t)
                        until (eq ev :done))
                  (ungrab-pointer))
                (update-configuration window)
                ;; don't forget to update the cache
                (setf (window-x window) (xlib:drawable-x (window-parent window))
                      (window-y window) (xlib:drawable-y (window-parent window)))))))))))

(defmethod group-button-press ((group float-group) x y where)
  (declare (ignore x y where))
  (when (next-method-p)
    (call-next-method)))

;;; Bindings

(pushnew '(float-group *float-group-top-map*) *group-top-maps*)
(defvar *float-group-top-map* (make-sparse-keymap))
(defvar *float-group-root-map* (make-sparse-keymap))


(defcommand gnew-float (name) ((:rest "Group Name: "))
  "Create a floating window group with the specified name and switch to it."
  (add-group (current-screen) name :type 'float-group))

(defcommand gnewbg-float (name) ((:rest "Group Name: "))
  "Create a floating window group with the specified name, but do not switch to it."
  (add-group (current-screen) name :background t :type 'float-group))
