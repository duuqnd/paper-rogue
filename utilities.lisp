;;;; utilities.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.utilities)

;; Misc. Utils

(defconstant +game-rand-modulo+ 281474976710656)

(defparameter *game-rand-start-seed* 1)
(defparameter *game-rand-current-seed* *game-rand-start-seed*)

(defun game-rand (modulo-number)
  (setf *game-rand-current-seed*
        (mod (+ (* 1103515245 *game-rand-current-seed*) 4294967)
             +game-rand-modulo+))
  (mod (ldb (byte 32 16) *game-rand-current-seed*) modulo-number))


