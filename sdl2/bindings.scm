;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-sdl2.
;;;
;;; Guile-sdl2 is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-sdl2 is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-sdl2.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Low-level FFI bindings.
;;
;;; Code:

(define-module (sdl2 bindings)
  #:use-module (system foreign)
  #:use-module (sdl2 config)
  #:export (boolean->sdl-bool

            SDL_INIT_TIMER
            SDL_INIT_AUDIO
            SDL_INIT_VIDEO
            SDL_INIT_HAPTIC
            SDL_INIT_GAMECONTROLLER
            SDL_INIT_EVENTS
            SDL_INIT_NOPARACHUTE

            SDL_WINDOW_FULLSCREEN
            SDL_WINDOW_OPENGL
            SDL_WINDOW_SHOWN
            SDL_WINDOW_HIDDEN
            SDL_WINDOW_BORDERLESS
            SDL_WINDOW_RESIZABLE
            SDL_WINDOW_MINIMIZED
            SDL_WINDOW_MAXIMIZED
            SDL_WINDOW_INPUT_GRABBED
            SDL_WINDOW_INPUT_FOCUS
            SDL_WINDOW_MOUSE_FOCUS
            SDL_WINDOW_FULLSCREEN_DESKTOP
            SDL_WINDOW_FOREIGN
            SDL_WINDOW_ALLOW_HIGHDPI
            SDL_WINDOW_MOUSE_CAPTURE))

(define sdl-func
  (let ((lib (dynamic-link %libsdl2)))
    (lambda (return-type function-name arg-types)
      "Return a procedure for the foreign function FUNCTION-NAME in
the SDL2 shared library.  That function must return a value of
RETURN-TYPE and accept arguments of ARG-TYPES."
      (pointer->procedure return-type
                          (dynamic-func function-name lib)
                          arg-types))))

(define-syntax-rule (define-foreign name return-type func-name arg-types)
  (define-public name
    (sdl-func return-type func-name arg-types)))


;;;
;;; Foreign Types
;;;

(define sdl-bool int)

(define (boolean->sdl-bool b)
  "Convert the boolean B to an SDL_bool."
  (if b 1 0))


;;;
;;; Errors
;;;

(define-foreign sdl-get-error
  '* "SDL_GetError" '())


;;;
;;; Initialization
;;;

(define SDL_INIT_TIMER          #x00000001)
(define SDL_INIT_AUDIO          #x00000010)
(define SDL_INIT_VIDEO          #x00000020)
(define SDL_INIT_HAPTIC         #x00001000)
(define SDL_INIT_GAMECONTROLLER #x00002000)
(define SDL_INIT_EVENTS         #x00004000)

(define-foreign sdl-init
  int "SDL_Init" (list uint32))

(define-foreign sdl-quit
  void "SDL_Quit" '())


;;;
;;; Version
;;;

(define-foreign sdl-get-version
  void "SDL_GetVersion" '(*))


;;;
;;; Video
;;;

(define SDL_WINDOW_FULLSCREEN         #x00000001)
(define SDL_WINDOW_OPENGL             #x00000002)
(define SDL_WINDOW_SHOWN              #x00000004)
(define SDL_WINDOW_HIDDEN             #x00000008)
(define SDL_WINDOW_BORDERLESS         #x00000010)
(define SDL_WINDOW_RESIZABLE          #x00000020)
(define SDL_WINDOW_MINIMIZED          #x00000040)
(define SDL_WINDOW_MAXIMIZED          #x00000080)
(define SDL_WINDOW_INPUT_GRABBED      #x00000100)
(define SDL_WINDOW_INPUT_FOCUS        #x00000200)
(define SDL_WINDOW_MOUSE_FOCUS        #x00000400)
(define SDL_WINDOW_FULLSCREEN_DESKTOP (logior SDL_WINDOW_FULLSCREEN
                                              #x00001000))
(define SDL_WINDOW_FOREIGN            #x00000800)
(define SDL_WINDOW_ALLOW_HIGHDPI      #x00002000)
(define SDL_WINDOW_MOUSE_CAPTURE      #x00004000)

(define-foreign sdl-create-window
  '* "SDL_CreateWindow" (list '* int int int int uint32))

(define-foreign sdl-destroy-window
  void "SDL_DestroyWindow" '(*))

(define-foreign sdl-get-window-title
  '* "SDL_GetWindowTitle" '(*))

(define-foreign sdl-get-window-size
  void "SDL_GetWindowSize" '(* * *))

(define-foreign sdl-get-window-position
  void "SDL_GetWindowPosition" '(* * *))

(define-foreign sdl-get-window-id
  uint32 "SDL_GetWindowID" '(*))

(define-foreign sdl-get-window-from-id
  '* "SDL_GetWindowFromID" (list uint32))

(define-foreign sdl-hide-window
  void "SDL_HideWindow" '(*))

(define-foreign sdl-show-window
  void "SDL_ShowWindow" '(*))

(define-foreign sdl-maximize-window
  void "SDL_MaximizeWindow" '(*))

(define-foreign sdl-minimize-window
  void "SDL_MinimizeWindow" '(*))

(define-foreign sdl-raise-window
  void "SDL_RaiseWindow" '(*))

(define-foreign sdl-restore-window
  void "SDL_RestoreWindow" '(*))

(define-foreign sdl-set-window-bordered
  void "SDL_SetWindowBordered" (list '* sdl-bool))

(define-foreign sdl-set-window-title
  void "SDL_SetWindowTitle" '(* *))

(define-foreign sdl-set-window-position
  void "SDL_SetWindowPosition" (list '* int int))

(define-foreign sdl-set-window-size
  void "SDL_SetWindowSize" (list '* int int))

(define-foreign sdl-gl-create-context
  '* "SDL_GL_CreateContext" '(*))

(define-foreign sdl-gl-delete-context
  void "SDL_GL_DeleteContext" '(*))

(define-foreign sdl-gl-swap-window
  void "SDL_GL_SwapWindow" '(*))


;;;
;;; Timer
;;;

(define-foreign sdl-get-ticks
  uint32 "SDL_GetTicks" '())
