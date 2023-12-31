;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2017 David Thompson <davet@gnu.org>
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
;; Keyboard input.
;;
;;; Code:

(define-module (sdl2 input keyboard)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (key-pressed?
            key-released?))

(define keycode-map
  (alist->hashq-table
   `((return . ,ffi:SDLK_RETURN)
     (escape . ,ffi:SDLK_ESCAPE)
     (backspace . ,ffi:SDLK_BACKSPACE)
     (tab . ,ffi:SDLK_TAB)
     (space . ,ffi:SDLK_SPACE)
     (exclaim . ,ffi:SDLK_EXCLAIM)
     (quotedbl . ,ffi:SDLK_QUOTEDBL)
     (hash . ,ffi:SDLK_HASH)
     (percent . ,ffi:SDLK_PERCENT)
     (dollar . ,ffi:SDLK_DOLLAR)
     (ampersand . ,ffi:SDLK_AMPERSAND)
     (quote . ,ffi:SDLK_QUOTE)
     (left-paren . ,ffi:SDLK_LEFTPAREN)
     (right-paren . ,ffi:SDLK_RIGHTPAREN)
     (asterisk . ,ffi:SDLK_ASTERISK)
     (plus . ,ffi:SDLK_PLUS)
     (comma . ,ffi:SDLK_COMMA)
     (minus . ,ffi:SDLK_MINUS)
     (period . ,ffi:SDLK_PERIOD)
     (slash . ,ffi:SDLK_SLASH)
     (0 . ,ffi:SDLK_0)
     (1 . ,ffi:SDLK_1)
     (2 . ,ffi:SDLK_2)
     (3 . ,ffi:SDLK_3)
     (4 . ,ffi:SDLK_4)
     (5 . ,ffi:SDLK_5)
     (6 . ,ffi:SDLK_6)
     (7 . ,ffi:SDLK_7)
     (8 . ,ffi:SDLK_8)
     (9 . ,ffi:SDLK_9)
     (colon . ,ffi:SDLK_COLON)
     (semicolon . ,ffi:SDLK_SEMICOLON)
     (less . ,ffi:SDLK_LESS)
     (equals . ,ffi:SDLK_EQUALS)
     (greater . ,ffi:SDLK_GREATER)
     (question . ,ffi:SDLK_QUESTION)
     (at . ,ffi:SDLK_AT)
     (left-bracket . ,ffi:SDLK_LEFTBRACKET)
     (backslash . ,ffi:SDLK_BACKSLASH)
     (right-bracket . ,ffi:SDLK_RIGHTBRACKET)
     (caret . ,ffi:SDLK_CARET)
     (underscore . ,ffi:SDLK_UNDERSCORE)
     (backquote . ,ffi:SDLK_BACKQUOTE)
     (a . ,ffi:SDLK_a)
     (b . ,ffi:SDLK_b)
     (c . ,ffi:SDLK_c)
     (d . ,ffi:SDLK_d)
     (e . ,ffi:SDLK_e)
     (f . ,ffi:SDLK_f)
     (g . ,ffi:SDLK_g)
     (h . ,ffi:SDLK_h)
     (i . ,ffi:SDLK_i)
     (j . ,ffi:SDLK_j)
     (k . ,ffi:SDLK_k)
     (l . ,ffi:SDLK_l)
     (m . ,ffi:SDLK_m)
     (n . ,ffi:SDLK_n)
     (o . ,ffi:SDLK_o)
     (p . ,ffi:SDLK_p)
     (q . ,ffi:SDLK_q)
     (r . ,ffi:SDLK_r)
     (s . ,ffi:SDLK_s)
     (t . ,ffi:SDLK_t)
     (u . ,ffi:SDLK_u)
     (v . ,ffi:SDLK_v)
     (w . ,ffi:SDLK_w)
     (x . ,ffi:SDLK_x)
     (y . ,ffi:SDLK_y)
     (z . ,ffi:SDLK_z)
     (caps-lock . ,ffi:SDLK_CAPSLOCK)
     (f1 . ,ffi:SDLK_F1)
     (f2 . ,ffi:SDLK_F2)
     (f3 . ,ffi:SDLK_F3)
     (f4 . ,ffi:SDLK_F4)
     (f5 . ,ffi:SDLK_F5)
     (f6 . ,ffi:SDLK_F6)
     (f7 . ,ffi:SDLK_F7)
     (f8 . ,ffi:SDLK_F8)
     (f9 . ,ffi:SDLK_F9)
     (f10 . ,ffi:SDLK_F10)
     (f11 . ,ffi:SDLK_F11)
     (f12 . ,ffi:SDLK_F12)
     (print-screen . ,ffi:SDLK_PRINTSCREEN)
     (scroll-lock . ,ffi:SDLK_SCROLLLOCK)
     (pause . ,ffi:SDLK_PAUSE)
     (insert . ,ffi:SDLK_INSERT)
     (home . ,ffi:SDLK_HOME)
     (page-up . ,ffi:SDLK_PAGEUP)
     (delete . ,ffi:SDLK_DELETE)
     (end . ,ffi:SDLK_END)
     (page-down . ,ffi:SDLK_PAGEDOWN)
     (right . ,ffi:SDLK_RIGHT)
     (left . ,ffi:SDLK_LEFT)
     (down . ,ffi:SDLK_DOWN)
     (up . ,ffi:SDLK_UP)
     (num-lock-clear . ,ffi:SDLK_NUMLOCKCLEAR)
     (keypad-divide . ,ffi:SDLK_KP_DIVIDE)
     (keypad-multiply . ,ffi:SDLK_KP_MULTIPLY)
     (keypad-minus . ,ffi:SDLK_KP_MINUS)
     (keypad-plus . ,ffi:SDLK_KP_PLUS)
     (keypad-enter . ,ffi:SDLK_KP_ENTER)
     (keypad-1 . ,ffi:SDLK_KP_1)
     (keypad-2 . ,ffi:SDLK_KP_2)
     (keypad-3 . ,ffi:SDLK_KP_3)
     (keypad-4 . ,ffi:SDLK_KP_4)
     (keypad-5 . ,ffi:SDLK_KP_5)
     (keypad-6 . ,ffi:SDLK_KP_6)
     (keypad-7 . ,ffi:SDLK_KP_7)
     (keypad-8 . ,ffi:SDLK_KP_8)
     (keypad-9 . ,ffi:SDLK_KP_9)
     (keypad-0 . ,ffi:SDLK_KP_0)
     (keypad-period . ,ffi:SDLK_KP_PERIOD)
     (application . ,ffi:SDLK_APPLICATION)
     (power . ,ffi:SDLK_POWER)
     (keypad-equals . ,ffi:SDLK_KP_EQUALS)
     (f13 . ,ffi:SDLK_F13)
     (f14 . ,ffi:SDLK_F14)
     (f15 . ,ffi:SDLK_F15)
     (f16 . ,ffi:SDLK_F16)
     (f17 . ,ffi:SDLK_F17)
     (f18 . ,ffi:SDLK_F18)
     (f19 . ,ffi:SDLK_F19)
     (f20 . ,ffi:SDLK_F20)
     (f21 . ,ffi:SDLK_F21)
     (f22 . ,ffi:SDLK_F22)
     (f23 . ,ffi:SDLK_F23)
     (f24 . ,ffi:SDLK_F24)
     (execute . ,ffi:SDLK_EXECUTE)
     (help . ,ffi:SDLK_HELP)
     (menu . ,ffi:SDLK_MENU)
     (select . ,ffi:SDLK_SELECT)
     (stop . ,ffi:SDLK_STOP)
     (again . ,ffi:SDLK_AGAIN)
     (undo . ,ffi:SDLK_UNDO)
     (cut . ,ffi:SDLK_CUT)
     (copy . ,ffi:SDLK_COPY)
     (paste . ,ffi:SDLK_PASTE)
     (find . ,ffi:SDLK_FIND)
     (mute . ,ffi:SDLK_MUTE)
     (volume-up . ,ffi:SDLK_VOLUMEUP)
     (volume-down . ,ffi:SDLK_VOLUMEDOWN)
     (keypad-comma . ,ffi:SDLK_KP_COMMA)
     (keypad-equals-as400 . ,ffi:SDLK_KP_EQUALSAS400)
     (alt-erase . ,ffi:SDLK_ALTERASE)
     (sysreq . ,ffi:SDLK_SYSREQ)
     (cancel . ,ffi:SDLK_CANCEL)
     (clear . ,ffi:SDLK_CLEAR)
     (prior . ,ffi:SDLK_PRIOR)
     (return2 . ,ffi:SDLK_RETURN2)
     (separator . ,ffi:SDLK_SEPARATOR)
     (out . ,ffi:SDLK_OUT)
     (oper . ,ffi:SDLK_OPER)
     (clear-again . ,ffi:SDLK_CLEARAGAIN)
     (crsel . ,ffi:SDLK_CRSEL)
     (exsel . ,ffi:SDLK_EXSEL)
     (keypad-00 . ,ffi:SDLK_KP_00)
     (keypad-000 . ,ffi:SDLK_KP_000)
     (thousands-separator . ,ffi:SDLK_THOUSANDSSEPARATOR)
     (decimal-separator . ,ffi:SDLK_DECIMALSEPARATOR)
     (currency-unit . ,ffi:SDLK_CURRENCYUNIT)
     (currency-subunit . ,ffi:SDLK_CURRENCYSUBUNIT)
     (keypad-left-paren . ,ffi:SDLK_KP_LEFTPAREN)
     (keypad-right-paren . ,ffi:SDLK_KP_RIGHTPAREN)
     (keypad-left-brace . ,ffi:SDLK_KP_LEFTBRACE)
     (keypad-right-brace . ,ffi:SDLK_KP_RIGHTBRACE)
     (keypad-tab . ,ffi:SDLK_KP_TAB)
     (keypad-backspace . ,ffi:SDLK_KP_BACKSPACE)
     (keypad-a . ,ffi:SDLK_KP_A)
     (keypad-b . ,ffi:SDLK_KP_B)
     (keypad-c . ,ffi:SDLK_KP_C)
     (keypad-d . ,ffi:SDLK_KP_D)
     (keypad-e . ,ffi:SDLK_KP_E)
     (keypad-f . ,ffi:SDLK_KP_F)
     (keypad-xor . ,ffi:SDLK_KP_XOR)
     (keypad-power . ,ffi:SDLK_KP_POWER)
     (keypad-percent . ,ffi:SDLK_KP_PERCENT)
     (keypad-less . ,ffi:SDLK_KP_LESS)
     (keypad-greater . ,ffi:SDLK_KP_GREATER)
     (keypad-ampersand . ,ffi:SDLK_KP_AMPERSAND)
     (keypad-dbl-ampersand . ,ffi:SDLK_KP_DBLAMPERSAND)
     (keypad-vertical-bar . ,ffi:SDLK_KP_VERTICALBAR)
     (keypad-dbl-vertical-bar . ,ffi:SDLK_KP_DBLVERTICALBAR)
     (keypad-colon . ,ffi:SDLK_KP_COLON)
     (keypad-hash . ,ffi:SDLK_KP_HASH)
     (keypad-space . ,ffi:SDLK_KP_SPACE)
     (keypad-at . ,ffi:SDLK_KP_AT)
     (keypad-exclam . ,ffi:SDLK_KP_EXCLAM)
     (keypad-mem-store . ,ffi:SDLK_KP_MEMSTORE)
     (keypad-mem-recall . ,ffi:SDLK_KP_MEMRECALL)
     (keypad-mem-clear . ,ffi:SDLK_KP_MEMCLEAR)
     (keypad-mem-add . ,ffi:SDLK_KP_MEMADD)
     (keypad-mem-subtract . ,ffi:SDLK_KP_MEMSUBTRACT)
     (keypad-mem-multiply . ,ffi:SDLK_KP_MEMMULTIPLY)
     (keypad-mem-divide . ,ffi:SDLK_KP_MEMDIVIDE)
     (keypad-plus-minus . ,ffi:SDLK_KP_PLUSMINUS)
     (keypad-clear . ,ffi:SDLK_KP_CLEAR)
     (keypad-clear-entry . ,ffi:SDLK_KP_CLEARENTRY)
     (keypad-binary . ,ffi:SDLK_KP_BINARY)
     (keypad-octal . ,ffi:SDLK_KP_OCTAL)
     (keypad-decimal . ,ffi:SDLK_KP_DECIMAL)
     (keypad-hexadecimal . ,ffi:SDLK_KP_HEXADECIMAL)
     (left-ctrl . ,ffi:SDLK_LCTRL)
     (left-shift . ,ffi:SDLK_LSHIFT)
     (left-alt . ,ffi:SDLK_LALT)
     (left-gui . ,ffi:SDLK_LGUI)
     (right-ctrl . ,ffi:SDLK_RCTRL)
     (right-shift . ,ffi:SDLK_RSHIFT)
     (right-alt . ,ffi:SDLK_RALT)
     (right-gui . ,ffi:SDLK_RGUI)
     (mode . ,ffi:SDLK_MODE)
     (audio-next . ,ffi:SDLK_AUDIONEXT)
     (audio-prev . ,ffi:SDLK_AUDIOPREV)
     (audio-stop . ,ffi:SDLK_AUDIOSTOP)
     (audio-play . ,ffi:SDLK_AUDIOPLAY)
     (audio-mute . ,ffi:SDLK_AUDIOMUTE)
     (media-select . ,ffi:SDLK_MEDIASELECT)
     (www . ,ffi:SDLK_WWW)
     (mail . ,ffi:SDLK_MAIL)
     (calculator . ,ffi:SDLK_CALCULATOR)
     (computer . ,ffi:SDLK_COMPUTER)
     (ac-search . ,ffi:SDLK_AC_SEARCH)
     (ac-home . ,ffi:SDLK_AC_HOME)
     (ac-back . ,ffi:SDLK_AC_BACK)
     (ac-forward . ,ffi:SDLK_AC_FORWARD)
     (ac-stop . ,ffi:SDLK_AC_STOP)
     (ac-refresh . ,ffi:SDLK_AC_REFRESH)
     (ac-bookmarks . ,ffi:SDLK_AC_BOOKMARKS)
     (brightness-down . ,ffi:SDLK_BRIGHTNESSDOWN)
     (brightness-up . ,ffi:SDLK_BRIGHTNESSUP)
     (display-switch . ,ffi:SDLK_DISPLAYSWITCH)
     (kbd-illum-toggle . ,ffi:SDLK_KBDILLUMTOGGLE)
     (kbd-illum-down . ,ffi:SDLK_KBDILLUMDOWN)
     (kbd-illum-up . ,ffi:SDLK_KBDILLUMUP)
     (eject . ,ffi:SDLK_EJECT)
     (sleep . ,ffi:SDLK_SLEEP))))

(define key-pressed?
  (let ((keystate (pointer->bytevector
                   (ffi:sdl-get-keyboard-state %null-pointer)
                   ffi:SDL_NUM_SCANCODES)))
    (lambda (key)
      "Return #t if KEY is currently being pressed."
      (let ((scancode (ffi:sdl-get-scancode-from-key
                       (hashq-ref keycode-map key))))
        (= 1 (u8vector-ref keystate scancode))))))

(define (key-released? key)
  "Return #t if KEY is not currently being pressed."
  (not (key-pressed? key)))
