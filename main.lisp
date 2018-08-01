(in-package #:ld42)

(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(gamekit:defgame ld42 () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "LD42"))

(defvar *black* (gamekit:vec4 0 0 0 0))
(defvar *blue* (gamekit:vec4 0 0 1 1))

(defvar *box-pos* (gamekit:vec2 0 0))

(defun real-time-seconds ()
  "Return seconds since point in time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (position time)
  "Update position vector depending on specified time"
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:x position) (+ 350 (* 100 (cos angle)))
          (gamekit:y position) (+ 250 (* 100 (sin angle))))))

;(defmethod gamekit:draw ((this ld42))
  ;(update-position *box-pos* (real-time-seconds))
  ;(gamekit:draw-rect *box-pos* 100 100 :fill-paint *blue*)
  ;(gamekit:draw-text "LD42" (gamekit:vec2 240.0 240.0)))

(defun main (argv)
  (declare (ignore argv))
  (gamekit:start 'ld42))


(defvar *dir* (concatenate 'string (uiop:getenv "LD42_ROOT") "/"))

(gamekit:register-resource-package :keyword (merge-pathnames "assets/" *dir*))

(gamekit:define-image :player "player.png")

(defmethod gamekit:draw ((app ld42))
  (update-position *box-pos* (real-time-seconds))
  (gamekit:draw-rect *box-pos* 100 100 :fill-paint *blue*)
  (gamekit:draw-text "LD42" (gamekit:vec2 240.0 240.0))
  (gamekit:draw-image *box-pos* :player))

(gamekit:define-sound :player-grab "test.wav")

(defmethod gamekit:post-initialize ((this ld42))
  (gamekit:bind-button :space :pressed
                       (lambda ()
                         (gamekit:play-sound :player-grab))))

