(use-modules (sdl2)
             (sdl2 video))

(sdl-init)

(define window (make-sdl-window))

(sleep 2)
(close-sdl-window! window)
(sdl-quit)
