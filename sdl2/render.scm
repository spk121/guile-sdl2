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
;; SDL 2D accelerated rendering.
;;
;;; Code:

(define-module (sdl2 render)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (make-renderer
            renderer?
            delete-renderer!
            call-with-renderer
            clear-renderer
            present-renderer
            render-copy
            set-render-draw-color

            surface->texture))


;;;
;;; Renderer
;;;

(define-wrapped-pointer-type <renderer>
  renderer?
  wrap-renderer unwrap-renderer
  (lambda (context port)
    (format port "#<renderer ~x>"
            (pointer-address (unwrap-renderer context)))))

(define (renderer-flags->bitmask flags)
  (apply logior
         (map (match-lambda
                ('software    ffi:SDL_RENDERER_SOFTWARE)
                ('accelerated ffi:SDL_RENDERER_ACCELERATED)
                ('vsync       ffi:SDL_RENDERER_PRESENTVSYNC)
                ('texture     ffi:SDL_RENDERER_TARGETTEXTURE))
              flags)))

(define* (make-renderer window #:optional (flags '(accelerated vsync)))
  "Return a new renderer for WINDOW created with the options specified
in FLAGS, a list of symbols.  The valid symbols that may appear in
FLAGS are:

* software, to use a software renderer fallback
* accelerated, to use hardware acceleration
* vsync, to synchronize rendering with the monitor's refresh rate
* texture, for render to texture support"
  (let ((ptr (ffi:sdl-create-renderer ((@@ (sdl2 video) unwrap-window)
                                       window)
                                      -1 ; pick driver automatically
                                      (renderer-flags->bitmask flags))))
    (if (null-pointer? ptr)
        (sdl-error "make-renderer" "failed to create renderer")
        (wrap-renderer ptr))))

(define (delete-renderer! renderer)
  "Delete the rendering context RENDERER."
  (ffi:sdl-destroy-renderer (unwrap-renderer renderer)))

(define (call-with-renderer renderer proc)
  "Call PROC, passing it RENDERER and closing RENDERER upon exit of
PROC."
  (dynamic-wind
    (const #t)
    (lambda ()
      (proc renderer))
    (lambda ()
      (delete-renderer! renderer))))

(define (clear-renderer renderer)
  "Clear the rendering target RENDERER with the current drawing
color."
  (unless (zero? (ffi:sdl-render-clear (unwrap-renderer renderer)))
    (sdl-error "clear-renderer!" "failed to clear renderer")))

(define (present-renderer renderer)
  "Display RENDERER."
  (ffi:sdl-render-present (unwrap-renderer renderer)))

(define (set-render-draw-color renderer r g b a)
  "Set draw color of RENDERER."
  (ffi:sdl-set-render-draw-color (unwrap-renderer renderer) r g b a))


;;;
;;; Texture
;;;

(define-wrapped-pointer-type <texture>
  texture?
  wrap-texture unwrap-texture
  (lambda (context port)
    (format port "#<texture ~x>"
            (pointer-address (unwrap-texture context)))))

(define (surface->texture renderer surface)
  "Convert SURFACE to a texture suitable for RENDERER."
  (let ((ptr (ffi:sdl-create-texture-from-surface
              (unwrap-renderer renderer)
              ((@@ (sdl2 surface) unwrap-surface) surface))))
    (if (null-pointer? ptr)
        (sdl-error "surface->texture" "failed to convert surface to texture")
        (wrap-texture ptr))))


(define* (render-copy renderer texture
                      #:key (angle 0) srcrect dstrect center)
  "Copy TEXTURE to the rendering target of RENDERER."
  (let ((result (ffi:sdl-render-copy-ex
                 (unwrap-renderer renderer)
                 (unwrap-texture texture)
                 (if srcrect
                     (make-c-struct ffi:sdl-rect srcrect)
                     %null-pointer)
                 (if dstrect
                     (make-c-struct ffi:sdl-rect dstrect)
                     %null-pointer)
                 angle
                 (if center
                     (make-c-struct ffi:sdl-point center)
                     %null-pointer)
                 0)))
    (unless (zero? result)
      (sdl-error "render-copy" "failed to copy texture"))))
