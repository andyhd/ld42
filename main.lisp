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
  (bopped nil)
  (speed 100))

(defvar *beavers* '())
(defvar *beavers-bopped* 0)
(defvar *black* (vec4 0 0 0 1))
(defvar *blue* (vec4 0.31 0.455 0.58 1))
(defvar *canvas-height* 480)
(defvar *canvas-width* 640)
(defvar *dam* (make-array '(15 20)))
(defvar *game-over* nil)
(defvar *keys* '())
(defvar *last-beaver* 0)
(defvar *last-tick* 0)
(defvar *mute* nil)
(defvar *player* (make-beaver :pos (vec2 320 240)))
(defvar *return-time* 20)
(defvar *show-title* t)
(defvar *steal-time* 5)
(defvar *time-to-next-beaver* 5)

(defgame ld42 () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "LD42"))

(register-resource-package :keyword (asdf:system-relative-pathname :ld42 "assets/"))

(define-image :player "beaver.png")
(define-image :title "title.png")
(define-sound :add-beaver "add-beaver.wav")
(define-sound :bop "bop.wav")
(define-sound :ouch "ouch.wav")

(defun init-dam (dam)
  (dotimes (y (array-dimension dam 0))
    (dotimes (x (array-dimension dam 1))
      (if (or (< x 3) (> x 16))
          (setf (aref dam y x) 2))
      (if (and (< 2 x 17)
               (< 3 y 11))
          (progn
            (setf (aref dam y x) 1)
            )))))

(defun bind-key (key)
  (setf *keys* (append *keys* (list (cons key 0))))
  (bind-button key :pressed (lambda () (rplacd (assoc key *keys*) 1)))
  (bind-button key :released (lambda () (rplacd (assoc key *keys*) 0))))

(defmethod post-initialize ((this ld42))
  (bind-key :up)
  (bind-key :down)
  (bind-key :left)
  (bind-key :right)
  (bind-key :space)
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

(defun tile-at (pos)
  (cond
    ((> 0 (x pos)) nil)
    ((> 0 (y pos)) nil)
    ((< 19 (x pos)) nil)
    ((< 14 (y pos)) nil)
    (t (aref *dam* (floor (y pos)) (floor (x pos))))))

(defun (setf tile-at) (new-value pos)
  (setf (aref *dam* (floor (y pos))  (floor (x pos))) new-value))

(defun collide (pos offset)
  (let ((test (add pos offset)))
    (or (< (x test) 0)
        (>= (x test) *canvas-width*)
        (< (y test) 0)
        (>= (y test) *canvas-height*)
        (zerop (tile-at (screen-to-map test))))))

(defun log-at (pos)
  (equal 1 (tile-at pos)))

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
  ; move straight towards the log unless the player is closer, then avoid
  (let* ((gdist (subt (beaver-goal b) (beaver-pos b)))
         (pdist (subt (beaver-pos *player*) (beaver-pos b)))
         (gsqdist (mult gdist gdist))
         (psqdist (mult pdist pdist))
         (ghyp (sqrt (+ (x gsqdist) (y gsqdist))))
         (phyp (sqrt (+ (x psqdist) (y psqdist))))
         ;(move (mult (div dist hyp) ticks (beaver-speed b)))
         )
    (setf (beaver-pos b)
          (add (cond
                 ((> ghyp phyp) (mult (div pdist phyp) -1 ticks (beaver-speed b)))
                 (t (mult (div gdist ghyp) ticks (beaver-speed b))))
               (beaver-pos b)))
    )
  )

(defun in-canvas (pos)
  (and (< -1 (+ (x pos) 32))
       (< -1 (+ (y pos) 32))
       (> (+ 1 *canvas-width*) (x pos))
       (> (+ 1 *canvas-height*) (y pos))))

(defun logs-remaining ()
  (let ((total 0))
    (dotimes (y (array-dimension *dam* 0))
      (dotimes (x (array-dimension *dam* 1))
        (if (or (equal 1 (tile-at (vec2 x y)))
                (equal 3 (tile-at (vec2 x y))))
            (incf total))))
    total))

(defun run-away (b ticks)
  ; remove the log from the dam
  (let ((goal (screen-to-map (beaver-goal b))))
    (if (equal 3 (tile-at goal))
        (setf (tile-at goal) 0)))
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
          (setf (beaver-busy-timer b) (start-timer *return-time*))
          ))
    )
  )

(defun take-log (b)
  ; remove log from dam
  (setf (tile-at (screen-to-map (beaver-goal b))) 3)
  (setf (beaver-has-log b) t)
  (setf (beaver-busy-timer b) (start-timer *steal-time*)))

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

(defun pressing (key)
  (not (zerop (cdr (assoc key *keys*)))))

(defun intersects (a b)
  (not (or (> (x a) (z b))
           (> (y a) (w b))
           (< (z a) (x b))
           (< (w a) (y b)))))

(defun touching (beaver)
  ; is the player touching a beaver?
  (let ((p (beaver-pos *player*))
        (b (beaver-pos beaver)))
    (intersects
      (vec4 (x p) (y p) (+ (x p) 32) (+ (y p) 32))
      (vec4 (x b) (y b) (+ (x b) 32) (+ (y b) 32)))))

(defun bop (beaver)
  ; bop a beaver on the head to knock him out
  (play-sound :bop)
  (setf (beaver-bopped beaver) t)
  (play-sound :ouch)
  (if (equal 3 (tile-at (beaver-goal beaver)))
      (setf (tile-at (beaver-goal beaver)) 1))
  (setf *beavers* (remove beaver *beavers*))
  (incf *beavers-bopped*)
  )

(defun handle-title-screen ()
  (if (and (pressing :space) *show-title*)
      (progn
        (setf *show-title* nil)
        (setf *game-over* nil)
        (setf *beavers* nil)
        (init-dam *dam*)
        )))

(defun handle-game-over ()
  (if (and (pressing :space) *game-over*)
      (progn
        (setf *show-title* t)
        (setf *game-over* nil)
        )))

(defun handle-game ()
  (if (and (>= 0 (logs-remaining))
           (not *game-over*))
      (setf *game-over* t))
  (let ((move (vec2 0 0))
        (offset (vec2 0 0))
        (ticks (get-ticks))
        (now (elapsed-seconds))
        )
    (if (< *time-to-next-beaver* (- now *last-beaver*))
        (progn
          (add-beaver)
          (setf *last-beaver* now)))
    (if (pressing :up)
        (progn
          (setf (y offset) 32)
          (incf (y move))))
    (if (pressing :down)
        (decf (y move)))
    (if (pressing :left)
        (decf (x move)))
    (if (pressing :right)
        (progn
          (setf (x offset) 32)
          (incf (x move))))
    (setf move (mult move ticks (beaver-speed *player*)))
    (if t
        (setf (beaver-pos *player*) (add move (beaver-pos *player*))))
    ; if you're touching a beaver, bop it
    (let ((victim (first (remove-if-not #'touching *beavers*))))
      (if (and victim (not (beaver-bopped victim)))
          (bop victim)))
    (dolist (b *beavers*)
      (update-beaver b ticks))))

(defmethod act ((app ld42))
  (cond
    (*show-title* (handle-title-screen))
    (*game-over* (handle-game-over))
    (t (handle-game))))

(defun draw-map ()
  (dotimes (y (array-dimension *dam* 0))
    (dotimes (x (array-dimension *dam* 1))
      (if (equal 2 (aref *dam* y x))
          (draw-image (vec2 (* 32 x) (* 32 y)) :player
                      :origin (vec2 0 128)
                      :width 32
                      :height 32))
      (if (or (equal 1 (aref *dam* y x))
              (equal 3 (aref *dam* y x))
              )
          (draw-image (vec2 (* 32 x) (* 32 y)) :player
                      :origin (vec2 0 64)
                      :width 32
                      :height 32)
          ))))

(defun draw-beaver (b &optional (origin (vec2 0 32)))
  (draw-image (beaver-pos b)
              :player
              :origin origin
              :width 32
              :height 32))

(defun get-digits (num)
  (cond
    ((> 0 num) nil)
    ((> 10 num) (list num))
    (t (append (get-digits (floor num 10)) (list (mod num 10))))))

(defun draw-score ()
  (draw-image (vec2 10 10) :player :origin (vec2 0 192) :width 96 :height 32)
  (let ((offset 0))
    (dolist (digit (get-digits (logs-remaining)))
      (draw-image (vec2 (+ 96 (* 18 offset)) 10) :player
                  :origin (vec2 (+ 96 (* 32 digit)) 192)
                  :width 32
                  :height 32)
      (incf offset)))
  )

(defun draw-game-screen ()
  (draw-rect
    (vec2 0 0) *canvas-width* *canvas-height*
    :fill-paint *blue*)
  (draw-map)
  (draw-beaver *player* (vec2 0 0))
  (mapcar #'draw-beaver *beavers*)
  (draw-score)
  )

(defun draw-title-screen ()
  (draw-image (vec2 0 0) :title))

(defun draw-game-over-screen ()
  (draw-game-screen)
  (draw-rect (vec2 0 0) *canvas-width* *canvas-height*
             :fill-paint (vec4 0 0 0 0.5))
  (draw-image (vec2 80 192) :player
              :origin (vec2 0 224)
              :width 480
              :height 96))

(defmethod draw ((app ld42))
  (cond
    (*show-title* (draw-title-screen))
    (*game-over* (draw-game-over-screen))
    (t (draw-game-screen))
    )
  )
