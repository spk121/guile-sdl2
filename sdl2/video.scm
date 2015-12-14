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
;; SDL display and window management functions.
;;
;;; Code:

(define-module (sdl2 video)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (window?
            make-window
            close-window!
            call-with-window
            window-title
            window-size
            window-position
            window-id
            id->window
            hide-window!
            show-window!
            maximize-window!
            minimize-window!
            raise-window!
            restore-window!
            set-window-border!
            set-window-title!
            set-window-position!
            set-window-size!

            make-gl-context
            gl-context?
            delete-gl-context!
            call-with-gl-context
            swap-gl-window))


;;;
;;; Windows
;;;

(define-wrapped-pointer-type <window>
  window?
  wrap-window unwrap-window
  (lambda (window port)
    (format port "#<window id: ~s title: ~s size: ~s position: ~s>"
            (window-id window)
            (window-title window)
            (window-size window)
            (window-position window))))

(define* (make-window #:key (title "Guile SDL2 Window")
                          (position '(0 0)) (size '(640 480))
                          (maximize? #f) (minimize? #f)
                          (show? #t) (resizable? #f)
                          (opengl? #f) (border? #t)
                          (fullscreen? #f) (fullscreen-desktop? #f)
                          (grab-input? #f) (high-dpi? #f))
  "Create a new window named TITLE with dimensions SIZE located at
POSITION on the display.  POSITION and SIZE are two-element lists in
the form '(x y)', where each coordinate is measured in pixels."
  (define x (match-lambda ((x _) x)))
  (define y (match-lambda ((_ y) y)))

  (let* ((flags (logior (if fullscreen?
                            ffi:SDL_WINDOW_FULLSCREEN
                            0)
                        (if fullscreen-desktop?
                            ffi:SDL_WINDOW_FULLSCREEN_DESKTOP
                            0)
                        (if opengl?
                            ffi:SDL_WINDOW_OPENGL
                            0)
                        (if show?
                            0
                            ffi:SDL_WINDOW_HIDDEN)
                        (if border?
                            0
                            ffi:SDL_WINDOW_BORDERLESS)
                        (if resizable?
                            ffi:SDL_WINDOW_RESIZABLE
                            0)
                        (if minimize?
                            ffi:SDL_WINDOW_MINIMIZED
                            0)
                        (if maximize?
                            ffi:SDL_WINDOW_MAXIMIZED
                            0)
                        (if grab-input?
                            ffi:SDL_WINDOW_INPUT_GRABBED
                            0)
                        (if high-dpi?
                            ffi:SDL_WINDOW_ALLOW_HIGHDPI
                            0)))
         (ptr (ffi:sdl-create-window (string->pointer title)
                                     (x position) (y position)
                                     (x size) (y size)
                                     flags)))
    (if (null-pointer? ptr)
        (sdl-error "make-window" "failed to create window")
        (wrap-window ptr))))

(define (close-window! window)
  "Close WINDOW."
  (ffi:sdl-destroy-window (unwrap-window window)))

(define (call-with-window window proc)
  "Call PROC with WINDOW, an SDL window object, and close it when PROC
returns or otherwise exits."
  (dynamic-wind
    (const #t)
    (lambda () (proc window))
    (lambda ()
      (close-window! window))))

(define (window-title window)
  "Return the title for WINDOW."
  (pointer->string (ffi:sdl-get-window-title (unwrap-window window))))

(define (%get-coords window proc)
  (let ((bv (make-bytevector (* 2 (sizeof int)) 0)))
    (proc (unwrap-window window)
          (bytevector->pointer bv)
          (bytevector->pointer bv (sizeof int)))
    (bytevector->sint-list bv (native-endianness) (sizeof int))))

(define (window-size window)
  "Return the dimensions of WINDOW."
  (%get-coords window ffi:sdl-get-window-size))

(define (window-position window)
  "Return the position of WINDOW on the display."
  (%get-coords window ffi:sdl-get-window-position))

(define (window-id window)
  "Return the numeric ID of WINDOW."
  (ffi:sdl-get-window-id (unwrap-window window)))

(define (id->window id)
  "Return the window corresponding to ID, a positive integer, or #f if
there is no such window."
  (let ((ptr (ffi:sdl-get-window-from-id id)))
    (if (null-pointer? ptr)
        #f
        (wrap-window ptr))))

(define (hide-window! window)
  "Hide WINDOW."
  (ffi:sdl-hide-window (unwrap-window window)))

(define (show-window! window)
  "Show WINDOW and focus on it."
  (ffi:sdl-show-window (unwrap-window window)))

(define (maximize-window! window)
  "Make WINDOW as large as possible."
  (ffi:sdl-maximize-window (unwrap-window window)))

(define (minimize-window! window)
  "Shrink WINDOW to an iconic representation."
  (ffi:sdl-minimize-window (unwrap-window window)))

(define (raise-window! window)
  "Raise WINDOW above all other windows and set input focus."
  (ffi:sdl-raise-window (unwrap-window window)))

(define (restore-window! window)
  "Restore the size and position of a minimized or maximized WINDOW."
  (ffi:sdl-restore-window (unwrap-window window)))

(define (set-window-border! window border?)
  "When BORDER?, draw the usual border around WINDOW, otherwise remove
the border."
  (ffi:sdl-set-window-bordered (unwrap-window window)
                               (ffi:boolean->sdl-bool border?)))

(define (set-window-title! window title)
  "Set the title of WINDOW to the string TITLE."
  (ffi:sdl-set-window-title (unwrap-window window)
                            (string->pointer title)))

(define (set-window-position! window position)
  "Set the position of WINDOW to POSITION, a two-element list of (x,y)
coordinates measured in pixels."
  (match position
    ((x y)
     (ffi:sdl-set-window-position (unwrap-window window) x y))))

(define (set-window-size! window size)
  "Set the dimensions of WINDOW to SIZE, a two-element list
of (width,height) coordinates measured in pixels."
  (match size
    ((width height)
     (ffi:sdl-set-window-size (unwrap-window window) width height))))


;;;
;;; OpenGL
;;;

(define-wrapped-pointer-type <gl-context>
  gl-context?
  wrap-gl-context unwrap-gl-context
  (lambda (context port)
    (format port "#<gl-context ~x>"
            (pointer-address (unwrap-gl-context context)))))

(define (make-gl-context window)
  "Create an OpenGL context for WINDOW."
  (let ((ptr (ffi:sdl-gl-create-context (unwrap-window window))))
    (if (null-pointer? ptr)
        (sdl-error "make-gl-context" "failed to create OpenGL context")
        (wrap-gl-context ptr))))

(define (delete-gl-context! context)
  "Delete CONTEXT, an OpenGL context object."
  (ffi:sdl-gl-delete-context (unwrap-gl-context context)))

(define (call-with-gl-context window proc)
  "Call PROC with a new OpenGL context created for WINDOW, and close
the context when PROC returns or otherwise exits.."
  (let ((context (make-gl-context window)))
    (dynamic-wind
      (const #t)
      (lambda () (proc context))
      (lambda ()
        (delete-gl-context! context)))))

(define (swap-gl-window window)
  "Update WINDOW with OpenGL rendering."
  (ffi:sdl-gl-swap-window (unwrap-window window)))
