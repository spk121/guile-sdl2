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
            surface-pixels))

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

;; The offsets below correspond to the SDL_Surface struct elements
;; that come before the element in question.
(define %width-offset (sizeof (list uint32 '*)))
(define %height-offset (sizeof (list uint32 '* int)))
(define %pitch-offset (sizeof (list uint32 '* int int)))
(define %pixels-offset 32)

(define (surface-width surface)
  "Return the width of SURFACE in pixels."
  (pointer-int-ref (unwrap-surface surface) %width-offset))

(define (surface-height surface)
  "Return the height of SURFACE in pixels."
  (pointer-int-ref (unwrap-surface surface) %height-offset))

(define (surface-pitch surface)
  "Return the length of a row of pixels in SURFACE in bytes."
  (pointer-int-ref (unwrap-surface surface) %pitch-offset))

(define (surface-pixels surface)
  "Return a bytevector containing the raw pixel data in SURFACE."
  (let* ((ptr (unwrap-surface surface))
         (height (pointer-int-ref ptr %height-offset))
         (pitch (pointer-int-ref ptr %pitch-offset))
         (pixels-ptr (pointer->bytevector ptr %pointer-size %pixels-offset))
         (pixels (make-pointer
                  (bytevector-uint-ref pixels-ptr
                                       0
                                       (native-endianness)
                                       %pointer-size))))
    (pointer->bytevector pixels (* height pitch))))
