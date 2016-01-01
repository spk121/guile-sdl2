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
;; SDL surface manipulation.
;;
;;; Code:

(define-module (sdl2 surface)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (surface?
            delete-surface!
            call-with-surface
            load-bmp
            surface-width
            surface-height
            surface-pitch
            surface-pixels

            convert-surface-format))

(define-wrapped-pointer-type <surface>
  surface?
  wrap-surface unwrap-surface
  (lambda (surface port)
    (format port "#<surface ~x>"
            (pointer-address (unwrap-surface surface)))))

(define (delete-surface! surface)
  "Free the memory used by SURFACE."
  (ffi:sdl-free-surface (unwrap-surface surface)))

(define (call-with-surface surface proc)
  "Call PROC, passing it SURFACE and deleting SURFACE upon exit of
PROC."
  (dynamic-wind
    (const #t)
    (lambda ()
      (proc surface))
    (lambda ()
      (delete-surface! surface))))

;; The equivalent of the SDL_LoadBMP C macro.
(define (load-bmp file)
  "Create a new surface from the bitmap data in FILE."
  (let ((ptr (ffi:sdl-load-bmp-rw (ffi:sdl-rw-from-file (string->pointer file)
                                                        (string->pointer "rb"))
                                  1)))
    (if (null-pointer? ptr)
        (sdl-error "load-bmp" "failed to load bitmap")
        (wrap-surface ptr))))

(define %int-size (sizeof int))
(define %pointer-size (sizeof '*))

(define (pointer-int-ref pointer offset)
  (bytevector-sint-ref (pointer->bytevector pointer %int-size offset)
                       0 (native-endianness) %int-size))

;; A partial list of surface types so that we can parse the data we
;; need out of the SDL_Surface struct pointer.
(define %surface-types
  (list uint32 ; flags
        '*     ; format
        int    ; width
        int    ; height
        int    ; pitch
        '*))   ; pixels

(define-syntax-rule (surface-parse-match surface matchers ...)
  (match (parse-c-struct (unwrap-surface surface) %surface-types)
    matchers ...))

(define (surface-width surface)
  "Return the width of SURFACE in pixels."
  (surface-parse-match surface
    ((_ _ width _ _ _) width)))

(define (surface-height surface)
  "Return the height of SURFACE in pixels."
    (surface-parse-match surface
      ((_ _ _ height _ _) height)))

(define (surface-pitch surface)
  "Return the length of a row of pixels in SURFACE in bytes."
  (surface-parse-match surface
    ((_ _ _ _ pitch _) pitch)))

(define (surface-pixels surface)
  "Return a bytevector containing the raw pixel data in SURFACE."
  (surface-parse-match surface
    ((_ _ _ height pitch pixels)
     (pointer->bytevector pixels (* height pitch)))))

(define (symbol->sdl-pixel-format sym)
  (match sym
    ('index1lsb   ffi:SDL_PIXELFORMAT_INDEX1LSB)
    ('index1msb   ffi:SDL_PIXELFORMAT_INDEX1MSB)
    ('index4lsb   ffi:SDL_PIXELFORMAT_INDEX4LSB)
    ('index4msb   ffi:SDL_PIXELFORMAT_INDEX4MSB)
    ('index8      ffi:SDL_PIXELFORMAT_INDEX8)
    ('rgb332      ffi:SDL_PIXELFORMAT_RGB332)
    ('rgb444      ffi:SDL_PIXELFORMAT_RGB444)
    ('rgb555      ffi:SDL_PIXELFORMAT_RGB555)
    ('bgr555      ffi:SDL_PIXELFORMAT_BGR555)
    ('argb4444    ffi:SDL_PIXELFORMAT_ARGB4444)
    ('rgba4444    ffi:SDL_PIXELFORMAT_RGBA4444)
    ('abgr4444    ffi:SDL_PIXELFORMAT_ABGR4444)
    ('bgra4444    ffi:SDL_PIXELFORMAT_BGRA4444)
    ('argb1555    ffi:SDL_PIXELFORMAT_ARGB1555)
    ('rgba5551    ffi:SDL_PIXELFORMAT_RGBA5551)
    ('abgr1555    ffi:SDL_PIXELFORMAT_ABGR1555)
    ('bgra5551    ffi:SDL_PIXELFORMAT_BGRA5551)
    ('rgb565      ffi:SDL_PIXELFORMAT_RGB565)
    ('bgr565      ffi:SDL_PIXELFORMAT_BGR565)
    ('rgb24       ffi:SDL_PIXELFORMAT_RGB24)
    ('bgr24       ffi:SDL_PIXELFORMAT_BGR24)
    ('rgb888      ffi:SDL_PIXELFORMAT_RGB888)
    ('rgbx8888    ffi:SDL_PIXELFORMAT_RGBX8888)
    ('bgr888      ffi:SDL_PIXELFORMAT_BGR888)
    ('bgrx8888    ffi:SDL_PIXELFORMAT_BGRX8888)
    ('argb8888    ffi:SDL_PIXELFORMAT_ARGB8888)
    ('rgba8888    ffi:SDL_PIXELFORMAT_RGBA8888)
    ('abgr8888    ffi:SDL_PIXELFORMAT_ABGR8888)
    ('bgra8888    ffi:SDL_PIXELFORMAT_BGRA8888)
    ('argb2101010 ffi:SDL_PIXELFORMAT_ARGB2101010)
    ('yv12        ffi:SDL_PIXELFORMAT_YV12)
    ('iyuv        ffi:SDL_PIXELFORMAT_IYUV)
    ('yuy2        ffi:SDL_PIXELFORMAT_YUY2)
    ('uyvy        ffi:SDL_PIXELFORMAT_UYVY)
    ('yvyu        ffi:SDL_PIXELFORMAT_YVYU)))

(define (convert-surface-format surface format)
  "Convert the pixels in SURFACE to FORMAT, a symbol representing a
specific pixel format, and return a new surface object.

Valid format types are:

- index1lsb
- index1msb
- index4lsb
- index4msb
- index8
- rgb332
- rgb444
- rgb555
- bgr555
- argb4444
- rgba4444
- abgr4444
- bgra4444
- argb1555
- rgba5551
- abgr1555
- bgra5551
- rgb565
- bgr565
- rgb24
- bgr24
- rgb888
- rgbx8888
- bgr888
- bgrx8888
- argb8888
- rgba8888
- abgr8888
- bgra8888
- argb2101010
- yv12
- iyuv
- yuy2
- uyvy
- yvyu"
  (let ((ptr (ffi:sdl-convert-surface-format (unwrap-surface surface)
                                             (symbol->sdl-pixel-format format)
                                             0)))
    (if (null-pointer? ptr)
        (sdl-error "convert-surface-format" "failed to convert surface format")
        (wrap-surface ptr))))
