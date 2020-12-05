;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2019 Pierre-Antoine Rouby <contact@parouby.fr>
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
  #:use-module (rnrs bytevectors)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (make-renderer
            renderer?
            delete-renderer!
            call-with-renderer
            clear-renderer
            present-renderer
            render-copy
            set-render-target!
            get-render-target
            set-render-draw-color
            set-render-draw-blend-mode
            render-draw-line
            render-draw-lines
            render-draw-point
            render-draw-points
            render-draw-rect
            render-draw-rects
            render-fill-rect
            render-fill-rects

            set-texture-color-mod!
            get-texture-color-mod
            set-texture-alpha-mod!
            get-texture-alpha-mod

            make-texture
            delete-texture!
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

(define (renderer-logical-size renderer)
  "Return the logical size used by RENDERER."
  (let ((bv (make-bytevector (* 2 (sizeof int)) 0)))
    (ffi:sdl-render-get-logical-size (unwrap-renderer renderer)
                                     (bytevector->pointer bv)
                                     (bytevector->pointer (sizeof int)))
    (bytevector->sint-list bv (native-endianness) (sizeof int))))

(define (set-renderer-logical-size! renderer width height)
  "Set the logical size of RENDERER to WIDTH x HEIGHT."
  (unless (zero? (ffi:sdl-render-set-logical-size
                  (unwrap-renderer renderer)
                  width height))
    (sdl-error "set-renderer-logical-size!" "Failed to set logical size")))

(define (renderer-scale renderer)
  "Return the scale used by RENDERER."
  (let ((bv (make-bytevector (* 2 (sizeof float)) 0)))
    (ffi:sdl-render-get-scale (unwrap-renderer renderer)
                              (bytevector->pointer bv)
                              (bytevector->pointer (sizeof float)))
    (list (bytevector-ieee-single-native-ref bv 0)
          (bytevector-ieee-single-native-ref bv 1))))

(define (set-renderer-scale! renderer scale-x scale-y)
  "Set the drawing scale of RENDERER according to SCALE-X and SCALE-Y
scaling factors."
  (unless (zero? (ffi:sdl-render-set-logical-size (unwrap-renderer renderer)
                                                  scale-x scale-y))
    (sdl-error "set-renderer-scale!" "Failed to set scale")))

(define (renderer-integer-scale renderer)
  "Returns #t if integer scaling is forced on RENDERER."
  (not
   (zero?
    (ffi:sdl-render-get-integer-scale
     (unwrap-renderer renderer)))))

(define (set-renderer-integer-scale! renderer enabled?)
  "If ENABLED? is #t, enable integer scaling for RENDERER, otherwise
disable it."
  (unless (zero? (ffi:sdl-render-set-integer-scale
                  (unwrap-renderer renderer)
                  (ffi:boolean->sdl-bool enabled?)))
    (sdl-error "set-renderer-integer-scale!" "Failed to set integer scale")))

(define (renderer-viewport renderer)
  "Return the drawing area used by RENDERER."
  (let ((rect ((@@ (sdl2 rect) make-rect) 0 0 0 0)))
    (ffi:sdl-render-get-viewport (unwrap-renderer renderer)
                                 ((@@ (sdl2 rect) unwrap-rect) rect))
    rect))

(define (set-renderer-viewport! renderer rect)
  "Set the drawing area for RENDERER to RECT."
  (unless (zero? (ffi:sdl-render-set-viewport
                  (unwrap-renderer renderer)
                  (if rect
                      ((@@ (sdl2 rect) unwrap-rect) rect)
                      %null-pointer)))
    (sdl-error "set-renderer-viewport!" "Failed to set viewport")))

(define (set-renderer-draw-blend-mode! renderer blend-mode)
  "Set blend mode of RENDERER to BLEND-MODE."
  (ffi:sdl-set-render-draw-blend-mode
   (unwrap-renderer renderer)
   ((@@ (sdl2 blend-mode) blend-mode-bitmask) blend-mode)))

(define (set-renderer-draw-color! renderer r g b a)
  "Set draw color of RENDERER."
  (ffi:sdl-set-render-draw-color (unwrap-renderer renderer) r g b a))

(define (draw-line renderer x1 y1 x2 y2)
  "Draw line on RENDERER."
  (ffi:sdl-render-draw-line (unwrap-renderer renderer) x1 y1 x2 y2))

(define (draw-rect renderer rect)
  "Draw RECT on RENDERER."
  (ffi:sdl-render-draw-rect
   (unwrap-renderer renderer)
   (if rect
       ((@@ (sdl2 rect) unwrap-rect) rect)
       %null-pointer)))

(define (draw-point renderer x y)
  "Draw point on RENDERER."
  (ffi:sdl-render-draw-point (unwrap-renderer renderer) x y))

(define *pointer-cache* (make-weak-key-hash-table))

(define (bytevector->pointer/cached bv)
  (or (hashq-ref *pointer-cache* bv)
      (let ((ptr (bytevector->pointer bv)))
        (hashq-set! *pointer-cache* bv ptr)
        ptr)))

(define (build-point-bv points)
  (let ((bv (make-s32vector (* (length points) 2))))
    (let loop ((points points)
               (i 0))
      (match points
        (() bv)
        (((x y) . rest)
         (s32vector-set! bv (* i 2) x)
         (s32vector-set! bv (+ (* i 1) 1) y)
         (loop rest (+ i 1)))))))

(define (draw-lines renderer points)
  "Draw lines connecting POINTS on RENDERER."
  (if (bytevector? points)
      (ffi:sdl-render-draw-lines (unwrap-renderer renderer)
                                 (bytevector->pointer/cached points)
                                 (/ (bytevector-length points) 8))
      (let ((bv (build-point-bv points)))
        (ffi:sdl-render-draw-lines (unwrap-renderer renderer)
                                   (bytevector->pointer bv)
                                   (length points)))))

(define (draw-points renderer points)
  "Draw POINTS on RENDERER."
  (if (bytevector? points)
      (ffi:sdl-render-draw-points (unwrap-renderer renderer)
                                  (bytevector->pointer/cached points)
                                  (/ (bytevector-length points) 8))
      (let ((bv (build-point-bv points)))
        (ffi:sdl-render-draw-points (unwrap-renderer renderer)
                                    (bytevector->pointer bv)
                                    (length points)))))

(define (build-rect-bv rects)
  (let* ((count (length rects))
         (bv (make-s32vector (* count 4))))
    (for-each (lambda (rect i)
                (bytevector-copy! ((@@ (sdl2 rect) rect-bv) rect) 0
                                  bv (* i 4 4) (* 4 4)))
              rects (iota count))
    bv))

(define (draw-rects renderer rects)
  "Draw RECTS on RENDERER."
  (if (bytevector? rects)
      (ffi:sdl-render-draw-rects (unwrap-renderer renderer)
                                 (bytevector->pointer/cached rects)
                                 (/ (bytevector-length rects) 16))
      (let ((bv (build-rect-bv rects)))
        (ffi:sdl-render-draw-rects (unwrap-renderer renderer)
                                   (bytevector->pointer bv)
                                   (length rects)))))

(define (fill-rect renderer rect)
  "Fill RECT on RENDERER."
  (ffi:sdl-render-fill-rect
   (unwrap-renderer renderer)
   (if rect
       ((@@ (sdl2 rect) unwrap-rect) rect)
       %null-pointer)))

(define (fill-rects renderer rects)
  "Fill RECTS on RENDERER."
  (if (bytevector? rects)
      (ffi:sdl-render-fill-rects (unwrap-renderer renderer)
                                 (bytevector->pointer/cached rects)
                                 (/ (bytevector-length rects) 16))
      (let ((bv (build-rect-bv rects)))
        (ffi:sdl-render-fill-rects (unwrap-renderer renderer)
                                   (bytevector->pointer bv)
                                   (length rects)))))


;;;
;;; Texture
;;;

(define-wrapped-pointer-type <texture>
  texture?
  wrap-texture unwrap-texture
  (lambda (context port)
    (format port "#<texture ~x>"
            (pointer-address (unwrap-texture context)))))

(define (make-texture renderer format access width height)
  "Returns a new texture for RENDERER with pixel FORMAT.
ACCESS is one of the symbols:

* static: changes rarely, not lockable
* streaming: changes frequently, lockable
* target: can be used as a render target (requires that renderer was
created with 'texture')"
  (let ((ptr (ffi:sdl-create-texture (unwrap-renderer renderer)
                                     ((@@ (sdl2 surface) symbol->sdl-pixel-format) format)
                                     (match access
                                       ('static    ffi:SDL_TEXTUREACCESS_STATIC)
                                       ('streaming ffi:SDL_TEXTUREACCESS_STREAMING)
                                       ('target    ffi:SDL_TEXTUREACCESS_TARGET))
                                     width height)))
    (if (null-pointer? ptr)
        (sdl-error "make-texture" "Failed to create texture")
        (wrap-texture ptr))))

(define (surface->texture renderer surface)
  "Convert SURFACE to a texture suitable for RENDERER."
  (let ((ptr (ffi:sdl-create-texture-from-surface
              (unwrap-renderer renderer)
              ((@@ (sdl2 surface) unwrap-surface) surface))))
    (if (null-pointer? ptr)
        (sdl-error "surface->texture" "failed to convert surface to texture")
        (wrap-texture ptr))))

(define (delete-texture! texture)
  "Free the memory used by TEXTURE."
  (ffi:sdl-destroy-texture (unwrap-texture texture)))

(define (set-texture-color-mod! texture r g b)
  "Get color mod of TEXTURE as a list of the integers."
  (unless (zero? (ffi:sdl-set-texture-color-mod (unwrap-texture texture) r g b))
    (sdl-error "set-texture-color-mod!" "Failed to set texture color mod")))

(define (set-texture-alpha-mod! texture a)
  "Sets alpha mod of TEXTURE."
  (unless (zero? (ffi:sdl-set-texture-alpha-mod (unwrap-texture texture) a))
    (sdl-error "set-texture-alpha-mod!" "Failed to set texture alpha mod")))

(define (texture-alpha-mod texture)
  "Get alpha mod of TEXTURE as a single integer."
  (let ((bv (make-bytevector 1)))
    (let ((result (ffi:sdl-get-texture-alpha-mod
                   (unwrap-texture texture)
                   (bytevector->pointer bv 0))))
      (unless (zero? result)
        (sdl-error "get-texture-alpha-mod" "Failed to get texture allpha mod"))

      (bytevector-u8-ref bv 0))))

(define (texture-color-mod texture)
  "Get color mod of TEXTURE as a list of the integers."
  (let ((bv (make-bytevector 3)))
    (let ((result (ffi:sdl-get-texture-color-mod
                   (unwrap-texture texture)
                   (bytevector->pointer bv 0)
                   (bytevector->pointer bv 1)
                   (bytevector->pointer bv 2))))
      (unless (zero? result)
        (sdl-error "get-texture-color-mod" "Failed to get texture color mod"))
      (bytevector->u8-list bv))))

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

(define (set-render-target! renderer texture)
  "Sets the render target for RENDERER to TEXTURE, making all comming draw
requests redirect to TEXTURE.

Pass #f to reset it to the default target."
  (let ((result (ffi:sdl-set-render-target
          (unwrap-renderer renderer)
          (if texture
              (unwrap-texture texture)
              %null-pointer))))
    (unless (zero? result)
      (sdl-error "set-render-target!" "failed to set render target"))))

(define (render-target renderer)
  "Returns the current render target of RENDERER. #f if it's a texture."
  (let ((ptr (ffi:sdl-get-render-target (unwrap-renderer renderer))))
    (if (null-pointer? ptr)
        #f (wrap-texture ptr))))
