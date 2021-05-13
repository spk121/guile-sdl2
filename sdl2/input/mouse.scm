;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2017, 2021 David Thompson <dthompson2@worcester.edu>
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
;; Mouse input.
;;
;;; Code:

(define-module (sdl2 input mouse)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (system foreign)
  #:export (mouse-x
            mouse-y
            mouse-button-pressed?
            mouse-button-released?
            set-show-cursor!
            cursor-visible?
            warp-mouse))

(define (make-int)
  (make-bytevector (sizeof int)))

(define (get-int bv)
  (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))

(define mouse-x
  (let* ((bv (make-int))
         (ptr (bytevector->pointer bv)))
    (lambda ()
      "Return the x coordinate of the mouse cursor."
      (ffi:sdl-get-mouse-state ptr %null-pointer)
      (get-int bv))))

(define mouse-y
  (let* ((bv (make-int))
         (ptr (bytevector->pointer bv)))
    (lambda ()
      "Return the y coordinate of the mouse cursor."
      (ffi:sdl-get-mouse-state %null-pointer ptr)
      (get-int bv))))

(define (mouse-button-pressed? button)
  "Return #t if BUTTON is currently being pressed."
  (let ((mask (match button
                ('left ffi:SDL_BUTTON_LMASK)
                ('middle ffi:SDL_BUTTON_MMASK)
                ('right ffi:SDL_BUTTON_RMASK)
                ('x1 ffi:SDL_BUTTON_X1MASK)
                ('x2 ffi:SDL_BUTTON_X2MASK)))
        (buttons (ffi:sdl-get-mouse-state %null-pointer %null-pointer)))
    (> (logand mask buttons) 0)))

(define (mouse-button-released? button)
  "Return #t if BUTTON is not currently being pressed."
  (not (mouse-button-pressed? button)))

(define (set-show-cursor! show?)
  "If SHOW? is #t, show the mouse cursor.  Otherwise, hide it."
  (when (< (ffi:sdl-show-cursor (if show? ffi:SDL_ENABLE ffi:SDL_DISABLE)) 0)
    (sdl-error "set-show-cursor!" "failed to modify cursor visibility")))

(define (cursor-visible?)
  "Return #t if the mouse cursor is currently visible."
  (= (ffi:sdl-show-cursor ffi:SDL_QUERY) ffi:SDL_ENABLE))

(define* (warp-mouse x y #:optional window)
  "Warp mouse cursor to (X, Y) relative to WINDOW.  If WINDOW is not
provided, the mouse cursor is moved to the global screen
coordinates (X, Y)."
  (if window
      (ffi:sdl-warp-mouse-in-window ((@@ (sdl2 video) unwrap-window) window)
                                    x y)
      (unless (= (ffi:sdl-warp-mouse-global x y) 0)
        (sdl-error "warp-mouse" "failed to warp mouse globally"))))
