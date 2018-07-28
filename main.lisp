(gamekit:defgame example () ())

(defmethod gamekit:draw ((this example))
  (gamekit:draw-text "Hello, Gamedev!" (gamekit:vec2 240.0 240.0)))

(defun main (argv)
  (declare (ignore argv))
  (gamekit:start 'example))
