(cl:defpackage :ld42
  (:use :cl :gamekit)
  (:export ld42))

(in-package :ld42)

(defstruct timer
  (countdown nil)
  (start nil)
  )

(defstruct beaver
  (pos (vec2 0 0))
  (goal (vec2 0 0))
  (has-log nil)
  (busy-timer nil)
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

(defun elapsed-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun start-timer (secs)
  (make-timer :countdown secs :start (elapsed-seconds)))

(defun timer-elapsed (timer)
  (floor (- (elapsed-seconds) (timer-start timer))))

(defun timer-ended (timer)
  (cond
    ((null timer) t)
    ((> (timer-elapsed timer) (timer-countdown timer)))))

(defun get-ticks ()
  (let* ((now (elapsed-seconds))
         (ticks (- now *last-tick*)))
    (setf *last-tick* now)
    ticks))

(defun screen-to-map (pos)
  (vec2 (floor (x pos) 32)
        (floor (y pos) 32)))

(defun collide (pos offset)
  (let ((test (add pos offset)))
    (or (< (x test) 0)
        (>= (x test) *canvas-width*)
        (< (y test) 0)
        (>= (y test) *canvas-height*)
        (let ((m (screen-to-map test)))
          (zerop (aref *dam* (floor (y m)) (floor (x m))))))))

(defun log-at (c)
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
    (if (log-at m)
        (setf (beaver-goal beaver) s)
        ; else
        (block :search
               (dotimes (d (- max-distance 1)) (incf d)
                 (dotimes (i (+ d 1))
                   (let ((c (vec2 (- (x m) (+ d i)) (- (y m) i))))
                     (if (log-at c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search))))
                   (let ((c (vec2 (+ (x m) (- d i)) (+ (y m) i))))
                     (if (log-at c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search)))))
                 (dotimes (i (- d 1)) (incf i)
                   (let ((c (vec2 (- (x m) i) (+ (y m) (- d i)))))
                     (if (log-at c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search))))
                   (let ((c (vec2 (+ (x m) (- d i)) (- (y m) i))))
                     (if (log-at c)
                         (progn
                           (setf (beaver-goal beaver) (mult c 32))
                           (return-from :search))))))))))

(defun add-beaver ()
  ; place a beaver at random on one of the screen edges
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
    ; add to the list of beavers
    (setf *beavers* (cons beaver *beavers*))
    ; say hello
    (or *mute* (play-sound :add-beaver))
    ; pick a log to steal
    (set-goal beaver)))

(defun reached-goal (b)
  ; check log hasn't been taken already
  (if (not (log-at (beaver-goal b)))
    ; pick another log
    (set-goal b))
  ; are we there yet?
  (let ((dist (subt (beaver-goal b) (beaver-pos b))))
    (and (< (abs (x dist)) 1)
         (< (abs (y dist)) 1)))
  )

(defun move-to-goal (b ticks)
  ; move straight towards the log
  (let* ((dist (subt (beaver-goal b) (beaver-pos b)))
         (sqdist (mult dist dist))
         (hyp (sqrt (+ (x sqdist) (y sqdist))))
         (move (mult (div dist hyp) ticks (beaver-speed b)))
         )
    (setf (beaver-pos b) (add move (beaver-pos b)))
    )
  )

(defun in-canvas (pos)
  (and (< -1 (+ (x pos) 32))
       (< -1 (+ (y pos) 32))
       (> (+ 1 *canvas-width*) (x pos))
       (> (+ 1 *canvas-height*) (y pos))))

(defun run-away (b ticks)
  ; move away from the player
  (let* ((dist (subt (beaver-pos *player*) (beaver-pos b)))
         (sqdist (mult dist dist))
         (hyp (sqrt (+ (x sqdist)  (y sqdist))))
         (move (mult (div dist hyp) ticks (beaver-speed b)))
         (dest (subt (beaver-pos b) move))
         )
    (if (in-canvas dest)
        (setf (beaver-pos b) dest)
        ; wait a while before coming back
        (progn
          (setf (beaver-has-log b) nil)
          (setf (beaver-busy-timer b) (start-timer 5))
          ))
    )
  )

(defun take-log (b)
  ; remove log from dam
  (let ((m (screen-to-map (beaver-goal b))))
    (setf (aref *dam* (floor (y m)) (floor (x m))) 0))
  (setf (beaver-has-log b) t)
  (setf (beaver-busy-timer b) (start-timer 3)))

(defun update-beaver (b ticks)
  (cond
    ; wait while busy removing log
    ((not (timer-ended (beaver-busy-timer b))) nil)
    ; if holding a log, run away
    ((beaver-has-log b) (run-away b ticks))
    ; steal a log
    ((reached-goal b) (take-log b))
    ; go find a log
    (t (move-to-goal b ticks))
    )
  )

(defmethod act ((app ld42))
  (let ((move (vec2 0 0))
        (offset (vec2 0 0))
        (ticks (get-ticks))
        (now (elapsed-seconds))
        )
    (if (< *time-to-next-beaver* (- now *last-beaver*))
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
