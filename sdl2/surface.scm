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
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (surface?
            delete-surface!
            call-with-surface
            load-bmp))

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
