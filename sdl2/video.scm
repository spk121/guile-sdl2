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
  #:export (sdl-window?
            make-sdl-window
            close-sdl-window!
            call-with-sdl-window
            sdl-window-title
            sdl-window-size
            sdl-window-position
            sdl-window-id
            id->sdl-window
            hide-sdl-window!
            show-sdl-window!
            maximize-sdl-window!
            minimize-sdl-window!
            raise-sdl-window!
            restore-sdl-window!
            set-sdl-window-border!
            set-sdl-window-title!
            set-sdl-window-position!
            set-sdl-window-size!

            make-gl-context
            gl-context?
            delete-gl-context!
            call-with-gl-context
            swap-gl-sdl-window))


;;;
;;; Windows
;;;

(define-wrapped-pointer-type <sdl-window>
  sdl-window?
  wrap-sdl-window unwrap-sdl-window
  (lambda (window port)
    (format port "#<sdl-window id: ~s title: ~s size: ~s position: ~s>"
            (sdl-window-id window)
            (sdl-window-title window)
            (sdl-window-size window)
            (sdl-window-position window))))

(define* (make-sdl-window #:key (title "Guile SDL2 Window")
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
        (sdl-error "make-sdl-window" "failed to create window")
        (wrap-sdl-window ptr))))

(define (close-sdl-window! window)
  "Close WINDOW."
  (ffi:sdl-destroy-window (unwrap-sdl-window window)))

(define (call-with-sdl-window args proc)
  "Call PROC with a new window defined by ARGS, a list of keyword
arguments accepted by 'make-sdl-window', and close it when PROC
returns or otherwise exits."
  (let ((window (apply make-sdl-window args)))
    (dynamic-wind
      (const #t)
      (lambda () (proc window))
      (lambda ()
        (close-sdl-window! window)))))

(define (sdl-window-title window)
  "Return the title for WINDOW."
  (pointer->string (ffi:sdl-get-window-title (unwrap-sdl-window window))))

(define (%get-coords window proc)
  (let ((bv (make-bytevector (* 2 (sizeof int)) 0)))
    (proc (unwrap-sdl-window window)
          (bytevector->pointer bv)
          (bytevector->pointer bv (sizeof int)))
    (bytevector->sint-list bv (native-endianness) (sizeof int))))

(define (sdl-window-size window)
  "Return the dimensions of WINDOW."
  (%get-coords window ffi:sdl-get-window-size))

(define (sdl-window-position window)
  "Return the position of WINDOW on the display."
  (%get-coords window ffi:sdl-get-window-position))

(define (sdl-window-id window)
  "Return the numeric ID of WINDOW."
  (ffi:sdl-get-window-id (unwrap-sdl-window window)))

(define (id->sdl-window id)
  "Return the window corresponding to ID, a positive integer, or #f if
there is no such window."
  (let ((ptr (ffi:sdl-get-window-from-id id)))
    (if (null-pointer? ptr)
        #f
        (wrap-sdl-window ptr))))

(define (hide-sdl-window! window)
  "Hide WINDOW."
  (ffi:sdl-hide-window (unwrap-sdl-window window)))

(define (show-sdl-window! window)
  "Show WINDOW and focus on it."
  (ffi:sdl-show-window (unwrap-sdl-window window)))

(define (maximize-sdl-window! window)
  "Make WINDOW as large as possible."
  (ffi:sdl-maximize-window (unwrap-sdl-window window)))

(define (minimize-sdl-window! window)
  "Shrink WINDOW to an iconic representation."
  (ffi:sdl-minimize-window (unwrap-sdl-window window)))

(define (raise-sdl-window! window)
  "Raise WINDOW above all other windows and set input focus."
  (ffi:sdl-raise-window (unwrap-sdl-window window)))

(define (restore-sdl-window! window)
  "Restore the size and position of a minimized or maximized WINDOW."
  (ffi:sdl-restore-window (unwrap-sdl-window window)))

(define (set-sdl-window-border! window border?)
  "When BORDER?, draw the usual border around WINDOW, otherwise remove
the border."
  (ffi:sdl-set-window-bordered (unwrap-sdl-window window)
                               (ffi:boolean->sdl-bool border?)))

(define (set-sdl-window-title! window title)
  "Set the title of WINDOW to the string TITLE."
  (ffi:sdl-set-window-title (unwrap-sdl-window window)
                            (string->pointer title)))

(define (set-sdl-window-position! window position)
  "Set the position of WINDOW to POSITION, a two-element list of (x,y)
coordinates measured in pixels."
  (match position
    ((x y)
     (ffi:sdl-set-window-position (unwrap-sdl-window window) x y))))

(define (set-sdl-window-size! window size)
  "Set the dimensions of WINDOW to SIZE, a two-element list
of (width,height) coordinates measured in pixels."
  (match size
    ((width height)
     (ffi:sdl-set-window-size (unwrap-sdl-window window) width height))))


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
  (let ((ptr (ffi:sdl-gl-create-context (unwrap-sdl-window window))))
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

(define (swap-gl-sdl-window window)
  "Update WINDOW with OpenGL rendering."
  (ffi:sdl-gl-swap-window (unwrap-sdl-window window)))
