(cl:defpackage :ld42
  (:use :cl :gamekit)
  (:export ld42))

(in-package :ld42)

(defstruct beaver
  (pos (vec2 0 0))
  (goal (vec2 0 0))
  (has-log nil)
  (busy-timer 0)
  (speed 100))

(defvar *beavers* '())
(defvar *black* (vec4 0 0 0 1))
(defvar *blue* (vec4 0.31 0.455 0.58 1))
(defvar *canvas-height* 480)
(defvar *canvas-width* 640)
(defvar *dam* (make-array '(15 20)))
(defvar *keys* '())
(defvar *last-beaver* 0)
(defvar *last-tick* 0)
(defvar *mute* nil)
(defvar *player* (make-beaver :pos (vec2 320 240)))
(defvar *time-to-next-beaver* 30)

(register-resource-package :keyword
                                   (asdf:system-relative-pathname :ld42 "assets/"))

(define-image :player "beaver.png")
(define-sound :add-beaver "add-beaver.wav")

(defgame ld42 () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "LD42"))

(defun init-dam (dam)
  (dotimes (y (array-dimension dam 0))
    (dotimes (x (array-dimension dam 1))
      (if (or (< x 3) (> x 16))
          (setf (aref dam y x) 2))
      (if (and (< 2 x 17)
               (< 1 y 13))
          (setf (aref dam y x) 1)))))

(defun bind-key (key)
  (setf *keys* (append *keys* (list (cons key 0))))
  (bind-button key :pressed (lambda () (rplacd (assoc key *keys*) 1)))
  (bind-button key :released (lambda () (rplacd (assoc key *keys*) 0))))

(defmethod post-initialize ((this ld42))
  (bind-key :up)
  (bind-key :down)
  (bind-key :left)
  (bind-key :right)
  (init-dam *dam*)
  )

(defun real-time-seconds ()
  "Return seconds since point in time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun get-ticks ()
  (let* ((now (real-time-seconds))
         (ticks (- now *last-tick*)))
    (setf *last-tick* now)
    ticks))

(defun collide (pos offset)
  (let ((test-x (+ (x pos) (x offset)))
        (test-y (+ (y pos) (y offset))))
    (or (< test-x 0)
        (>= test-x *canvas-width*)
        (< test-y 0)
        (>= test-y *canvas-height*)
        (let ((map-x (floor test-x 32))
              (map-y (floor test-y 32)))
          (zerop (aref *dam* map-y map-x))))))

(defun is-log (c)
  (cond
    ((> 0 (x c)) nil)
    ((> 0 (y c)) nil)
    ((< 19 (x c)) nil)
    ((< 14 (y c)) nil)
    (t (equal 1 (aref *dam* (floor (y c)) (floor (x c)))))))

(defun set-goal (beaver)
  (let* ((s (beaver-pos beaver))
         (m (vec2 (floor (x s) 32) (floor (y s) 32)))
         (max-distance 20))
    (if (is-log m)
        (setf (beaver-goal beaver) s)
        ; else
        (block :search
               (dotimes (d (- max-distance 1))
                 (incf d) ; for d=1; i < maxdistance
                 (dotimes (i (+ d 1)) ; for i=0; i < d + 1
                   (let ((c (vec2 (- (x m) (+ d i)) (- (y m) i))))
                     (if (is-log c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search))))
                   (let ((c (vec2 (+ (x m) (- d i)) (+ (y m) i))))
                     (if (is-log c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search)))))
                 (dotimes (i (- d 1)) ; for i=1; i < d
                   (incf i)
                   (let ((c (vec2 (- (x m) i) (+ (y m) (- d i)))))
                     (if (is-log c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search))))
                   (let ((c (vec2 (+ (x m) (- d i)) (- (y m) i))))
                     (if (is-log c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search))))))))))

(defun add-beaver ()
  (let* ((edge (random 4))
         (pos (vec2
                (case edge
                  (0 0)
                  (2 (- *canvas-width* 32))
                  (otherwise (random *canvas-width*)))
                (case edge
                  (1 0)
                  (3 (- *canvas-height* 32))
                  (otherwise (random *canvas-height*)))))
         (beaver (make-beaver :pos pos)))
    (setf *beavers* (cons beaver *beavers*))
    (or *mute* (play-sound :add-beaver))
    (set-goal beaver)))

(defun reached-goal (b)
  (let ((dist (subt (beaver-goal b) (beaver-pos b))))
    (and (< (abs (x dist)) 1)
         (< (abs (y dist)) 1)))
  )

(defun move-to-goal (b ticks)
  (let* ((dist (subt (beaver-goal b) (beaver-pos b)))
         (sqdist (mult dist dist))
         (hyp (sqrt (+ (x sqdist) (y sqdist))))
         (move (mult (div dist hyp) ticks (beaver-speed b)))
         )
    (setf (beaver-pos b) (add move (beaver-pos b)))
    )
  )

(defun update-beaver (b ticks)
  (if (not (reached-goal b))
      (move-to-goal b ticks)
      ;(take-log b)
      )
  )

(defmethod act ((app ld42))
  (let ((move (vec2 0 0))
        (offset (vec2 0 0))
        (ticks (get-ticks))
        (now (real-time-seconds))
        )
    (if (< *time-to-next-beaver*
           (- now *last-beaver*))
        (progn
          ;(add-beaver)
          (setf *last-beaver* now)))
    (if (not (zerop (cdr (assoc :up *keys*))))
        (progn
          (setf (y offset) 32)
          (incf (y move))))
    (if (not (zerop (cdr (assoc :down *keys*))))
        (decf (y move)))
    (if (not (zerop (cdr (assoc :left *keys*))))
        (decf (x move)))
    (if (not (zerop (cdr (assoc :right *keys*))))
        (progn
          (setf (x offset) 32)
          (incf (x move))))
    (setf move (mult move ticks (beaver-speed *player*)))
    (if (not (collide (add move (beaver-pos *player*)) offset))
        (setf (beaver-pos *player*) (add move (beaver-pos *player*))))
    (dolist (b *beavers*)
      (update-beaver b ticks))))

(defun draw-map ()
  (dotimes (y (array-dimension *dam* 0))
    (dotimes (x (array-dimension *dam* 1))
      (if (equal 2 (aref *dam* y x))
          (draw-image (vec2 (* 32 x) (* 32 y)) :player
                      :origin (vec2 0 128)
                      :width 32
                      :height 32))
      (if (equal 1 (aref *dam* y x))
          (draw-image (vec2 (* 32 x) (* 32 y)) :player
                      :origin (vec2 0 96)
                      :width 32
                      :height 32)
          ))))

(defun draw-beaver (b &optional (origin (vec2 0 32)))
  (draw-image (beaver-pos b)
              :player
              :origin origin
              :width 32
              :height 32))

(defmethod draw ((app ld42))
  (draw-rect
    (vec2 0 0) *canvas-width* *canvas-height*
    :fill-paint *blue*)
  (draw-map)
  (draw-beaver *player* (vec2 0 0))
  (mapcar #'draw-beaver *beavers*)
  )
