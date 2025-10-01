;;; cube.el --- 3D Rubik's Cube -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-19
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (eagle "0"))
;; Keywords: games
;; Homepage: https://codeberg.org/akib/emacs-cube

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO.

;;; Code:

(require 'color)
(require 'eagle)
(require 'cl-lib)
(require 'bookmark)

(defgroup cube nil
  "3D Rubik's Cube in Emacs."
  :group 'games
  :prefix "cube-"
  :link '(url-link "https://codeberg.org/akib/emacs-cube"))

(defcustom cube-camera-distance 7.5
  "Distance of camera from the center of cube."
  :type 'float)

(defcustom cube-camera-rotation-amount 5.0
  "Amount of camera rotation in degree."
  :type 'float)

(defcustom cube-resolution '(64 . 64)
  "Resolution for rendering cube.

Increase to improve render quality, decrease to improve performance.

Just changing the value using programs has no effect if the cube has
already been rendered before,  call `cube-update-framebuffer' after
changing it."
  :type '(cons (integer :tag "Width")
               (integer :tag "Height"))
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (fboundp #'cube-update-framebuffer)
                    (bound-and-true-p cube--framebuffer))
           (cube-update-framebuffer))))

(defcustom cube-scale 10
  "Scale the cube this times."
  :type 'integer)

(defcustom cube-animation-steps 3
  "Number of step of animation to show while rotating a face.

Value should be a integer greater than zero.  A value of 1 means don't
animate, show the new state after rotation instantly."
  :type 'integer)

(defcustom cube-animation-duration 0.5
  "Duration of animatation in seconds."
  :type 'number)

(defcustom cube-animation-break 0.2
  "Duration of time in seconds to break between two moves."
  :type 'number)

(defcustom cube-animate-on-shuffle t
  "When non-nil, animate moves while shuffling."
  :type 'boolean)

(defface cube-white
  '((t :foreground "white"))
  "Color of the white colored face of cube.")

(defface cube-yellow
  '((t :foreground "yellow"))
  "Color of the yellow colored face of cube.")

(defface cube-red
  '((t :foreground "red"))
  "Color of the red colored face of cube.")

(defface cube-green
  '((t :foreground "green"))
  "Color of the green colored face of cube.")

(defface cube-blue
  '((t :foreground "blue"))
  "Color of the blue colored face of cube.")

(defface cube-orange
  '((t :foreground "orange"))
  "Color of the orange colored face of cube.")

(defface cube-background
  '((t :foreground "black"))
  "Background color of cube.")

(defun cube--piece (color-front color-back color-left color-right color-up
                                color-down)
  "Make a piece.

COLOR-FRONT, COLOR-BACK, COLOR-LEFT, COLOR-RIGHT, COLOR-UP and COLOR-DOWN
defines the color of the piece."
  (let ((border 0.2))
    `(,@(if color-front
            `([-1.0 1.0 1.0 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [,(- border 1) ,(- border 1) 1.0 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [,(- border 1) ,(- border 1) 1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) 1.0 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [,(- 1 border) ,(- border 1) 1.0 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [,(- 1 border) ,(- border 1) 1.0 1.0 cube-background]
              [,(- border 1) ,(- border 1) 1.0 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [,(- 1 border) ,(- 1 border) 1.0 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [,(- 1 border) ,(- 1 border) 1.0 1.0 cube-background]
              [,(- 1 border) ,(- border 1) 1.0 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) 1.0 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) 1.0 1.0 cube-background]
              [,(- 1 border) ,(- 1 border) 1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) 1.0 1.0 ,color-front]
              [,(- border 1) ,(- border 1) 1.0 1.0 ,color-front]
              [,(- 1 border) ,(- border 1) 1.0 1.0 ,color-front]
              [,(- border 1) ,(- 1 border) 1.0 1.0 ,color-front]
              [,(- 1 border) ,(- border 1) 1.0 1.0 ,color-front]
              [,(- 1 border) ,(- 1 border) 1.0 1.0 ,color-front])
          `([1.0 1.0 1.0 1.0 cube-background]
            [-1.0 1.0 1.0 1.0 cube-background]
            [1.0 -1.0 1.0 1.0 cube-background]
            [1.0 -1.0 1.0 1.0 cube-background]
            [-1.0 1.0 1.0 1.0 cube-background]
            [-1.0 -1.0 1.0 1.0 cube-background]))
      ,@(if color-back
            `([-1.0 1.0 -1.0 1.0 cube-background]
              [,(- border 1) ,(- border 1) -1.0 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) -1.0 1.0 cube-background]
              [,(- border 1) ,(- border 1) -1.0 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [,(- 1 border) ,(- border 1) -1.0 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [,(- border 1) ,(- border 1) -1.0 1.0 cube-background]
              [,(- 1 border) ,(- border 1) -1.0 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [,(- 1 border) ,(- 1 border) -1.0 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [,(- 1 border) ,(- border 1) -1.0 1.0 cube-background]
              [,(- 1 border) ,(- 1 border) -1.0 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) -1.0 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [,(- 1 border) ,(- 1 border) -1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) -1.0 1.0 cube-background]
              [,(- border 1) ,(- 1 border) -1.0 1.0 ,color-back]
              [,(- 1 border) ,(- border 1) -1.0 1.0 ,color-back]
              [,(- border 1) ,(- border 1) -1.0 1.0 ,color-back]
              [,(- border 1) ,(- 1 border) -1.0 1.0 ,color-back]
              [,(- 1 border) ,(- 1 border) -1.0 1.0 ,color-back]
              [,(- 1 border) ,(- border 1) -1.0 1.0 ,color-back])
          `([1.0 1.0 -1.0 1.0 cube-background]
            [1.0 -1.0 -1.0 1.0 cube-background]
            [-1.0 1.0 -1.0 1.0 cube-background]
            [1.0 -1.0 -1.0 1.0 cube-background]
            [-1.0 -1.0 -1.0 1.0 cube-background]
            [-1.0 1.0 -1.0 1.0 cube-background]))
      ,@(if color-left
            `([-1.0 -1.0 1.0 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- border 1) 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- 1 border) 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- border 1) 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [-1.0 ,(- 1 border) ,(- border 1) 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- border 1) 1.0 cube-background]
              [-1.0 ,(- 1 border) ,(- border 1) 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [-1.0 ,(- 1 border) ,(- 1 border) 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [-1.0 ,(- 1 border) ,(- border 1) 1.0 cube-background]
              [-1.0 ,(- 1 border) ,(- 1 border) 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- 1 border) 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [-1.0 ,(- 1 border) ,(- 1 border) 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- 1 border) 1.0 cube-background]
              [-1.0 ,(- border 1) ,(- 1 border) 1.0 ,color-left]
              [-1.0 ,(- 1 border) ,(- border 1) 1.0 ,color-left]
              [-1.0 ,(- border 1) ,(- border 1) 1.0 ,color-left]
              [-1.0 ,(- border 1) ,(- 1 border) 1.0 ,color-left]
              [-1.0 ,(- 1 border) ,(- 1 border) 1.0 ,color-left]
              [-1.0 ,(- 1 border) ,(- border 1) 1.0 ,color-left])
          `([-1.0 1.0 1.0 1.0 cube-background]
            [-1.0 1.0 -1.0 1.0 cube-background]
            [-1.0 -1.0 1.0 1.0 cube-background]
            [-1.0 1.0 -1.0 1.0 cube-background]
            [-1.0 -1.0 -1.0 1.0 cube-background]
            [-1.0 -1.0 1.0 1.0 cube-background]))
      ,@(if color-right
            `([1.0 -1.0 1.0 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [1.0 ,(- border 1) ,(- border 1) 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [1.0 ,(- border 1) ,(- border 1) 1.0 cube-background]
              [1.0 ,(- border 1) ,(- 1 border) 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [1.0 ,(- 1 border) ,(- border 1) 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [1.0 ,(- 1 border) ,(- border 1) 1.0 cube-background]
              [1.0 ,(- border 1) ,(- border 1) 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [1.0 ,(- 1 border) ,(- 1 border) 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [1.0 ,(- 1 border) ,(- 1 border) 1.0 cube-background]
              [1.0 ,(- 1 border) ,(- border 1) 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [1.0 ,(- border 1) ,(- 1 border) 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [1.0 ,(- border 1) ,(- 1 border) 1.0 cube-background]
              [1.0 ,(- 1 border) ,(- 1 border) 1.0 cube-background]
              [1.0 ,(- border 1) ,(- 1 border) 1.0 ,color-right]
              [1.0 ,(- border 1) ,(- border 1) 1.0 ,color-right]
              [1.0 ,(- 1 border) ,(- border 1) 1.0 ,color-right]
              [1.0 ,(- border 1) ,(- 1 border) 1.0 ,color-right]
              [1.0 ,(- 1 border) ,(- border 1) 1.0 ,color-right]
              [1.0 ,(- 1 border) ,(- 1 border) 1.0 ,color-right])
          `([1.0 1.0 1.0 1.0 cube-background]
            [1.0 -1.0 1.0 1.0 cube-background]
            [1.0 1.0 -1.0 1.0 cube-background]
            [1.0 1.0 -1.0 1.0 cube-background]
            [1.0 -1.0 1.0 1.0 cube-background]
            [1.0 -1.0 -1.0 1.0 cube-background]))
      ,@(if color-up
            `([-1.0 1.0 1.0 1.0 cube-background]
              [,(- border 1) 1.0 ,(- border 1) 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [,(- border 1) 1.0 ,(- 1 border) 1.0 cube-background]
              [,(- border 1) 1.0 ,(- border 1) 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [,(- 1 border) 1.0 ,(- border 1) 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [-1.0 1.0 -1.0 1.0 cube-background]
              [,(- border 1) 1.0 ,(- border 1) 1.0 cube-background]
              [,(- 1 border) 1.0 ,(- border 1) 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [,(- 1 border) 1.0 ,(- 1 border) 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [1.0 1.0 -1.0 1.0 cube-background]
              [,(- 1 border) 1.0 ,(- border 1) 1.0 cube-background]
              [,(- 1 border) 1.0 ,(- 1 border) 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [,(- border 1) 1.0 ,(- 1 border) 1.0 cube-background]
              [-1.0 1.0 1.0 1.0 cube-background]
              [1.0 1.0 1.0 1.0 cube-background]
              [,(- 1 border) 1.0 ,(- 1 border) 1.0 cube-background]
              [,(- border 1) 1.0 ,(- 1 border) 1.0 cube-background]
              [,(- border 1) 1.0 ,(- 1 border) 1.0 ,color-up]
              [,(- 1 border) 1.0 ,(- border 1) 1.0 ,color-up]
              [,(- border 1) 1.0 ,(- border 1) 1.0 ,color-up]
              [,(- border 1) 1.0 ,(- 1 border) 1.0 ,color-up]
              [,(- 1 border) 1.0 ,(- 1 border) 1.0 ,color-up]
              [,(- 1 border) 1.0 ,(- border 1) 1.0 ,color-up])
          `([1.0 1.0 1.0 1.0 cube-background]
            [1.0 1.0 -1.0 1.0 cube-background]
            [-1.0 1.0 1.0 1.0 cube-background]
            [1.0 1.0 -1.0 1.0 cube-background]
            [-1.0 1.0 -1.0 1.0 cube-background]
            [-1.0 1.0 1.0 1.0 cube-background]))
      ,@(if color-down
            `([-1.0 -1.0 1.0 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [,(- border 1) -1.0 ,(- border 1) 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [,(- border 1) -1.0 ,(- border 1) 1.0 cube-background]
              [,(- border 1) -1.0 ,(- 1 border) 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [,(- 1 border) -1.0 ,(- border 1) 1.0 cube-background]
              [-1.0 -1.0 -1.0 1.0 cube-background]
              [,(- 1 border) -1.0 ,(- border 1) 1.0 cube-background]
              [,(- border 1) -1.0 ,(- border 1) 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [,(- 1 border) -1.0 ,(- 1 border) 1.0 cube-background]
              [1.0 -1.0 -1.0 1.0 cube-background]
              [,(- 1 border) -1.0 ,(- 1 border) 1.0 cube-background]
              [,(- 1 border) -1.0 ,(- border 1) 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [-1.0 -1.0 1.0 1.0 cube-background]
              [,(- border 1) -1.0 ,(- 1 border) 1.0 cube-background]
              [1.0 -1.0 1.0 1.0 cube-background]
              [,(- border 1) -1.0 ,(- 1 border) 1.0 cube-background]
              [,(- 1 border) -1.0 ,(- 1 border) 1.0 cube-background]
              [,(- border 1) -1.0 ,(- 1 border) 1.0 ,color-down]
              [,(- border 1) -1.0 ,(- border 1) 1.0 ,color-down]
              [,(- 1 border) -1.0 ,(- border 1) 1.0 ,color-down]
              [,(- border 1) -1.0 ,(- 1 border) 1.0 ,color-down]
              [,(- 1 border) -1.0 ,(- border 1) 1.0 ,color-down]
              [,(- 1 border) -1.0 ,(- 1 border) 1.0 ,color-down])
          `([1.0 -1.0 1.0 1.0 cube-background]
            [-1.0 -1.0 1.0 1.0 cube-background]
            [1.0 -1.0 -1.0 1.0 cube-background]
            [1.0 -1.0 -1.0 1.0 cube-background]
            [-1.0 -1.0 1.0 1.0 cube-background]
            [-1.0 -1.0 -1.0 1.0 cube-background])))))

(defmacro cube--defpiece (name position color-front color-back color-left
                               color-right color-up color-down docstring)
  "Define a piece named NAME at POSITION.

COLOR-FRONT, COLOR-BACK, COLOR-LEFT, COLOR-RIGHT, COLOR-UP and COLOR-DOWN
defines the colors the piece.

DOCSTRING should be a string describing the piece."
  (declare (indent 2)
           (doc-string 9))
  `(progn
     (defconst ,(intern (format "cube--%S-piece" name))
       (let ((vertices nil)
             (matrix (eagle-translate (eagle-scale [[1.0 0.0 0.0 0.0]
                                                    [0.0 1.0 0.0 0.0]
                                                    [0.0 0.0 1.0 0.0]
                                                    [0.0 0.0 0.0 1.0]]
                                                   [0.5 0.5 0.5])
                                      ,position)))
         (dolist (vertex (cube--piece ,color-front ,color-back ,color-left
                                      ,color-right ,color-up ,color-down))
           (push
            `[,@(append
                 (eagle-multiply
                  matrix
                  (vector (aref vertex 0) (aref vertex 1) (aref vertex 2)
                          (aref vertex 3)))
                 nil)
              ,(aref vertex 4)]
            vertices))
         (apply #'vector (nreverse vertices)))
       ,(format "%s" docstring))
     (defconst ,(intern (format "cube--%S-piece-position" name))
       [,(aref position 0) ,(aref position 1) ,(aref position 2) 1.0]
       ,(format "Position of %s%s" (downcase (substring docstring 0 1))
                (substring docstring 1)))
     (defvar-local ,(intern (format "cube--%S-piece-rotation" name))
       [[1.0 0.0 0.0 0.0]
        [0.0 1.0 0.0 0.0]
        [0.0 0.0 1.0 0.0]
        [0.0 0.0 0.0 1.0]]
       ,(format "Rotation matrix for %s%s"
                (downcase (substring docstring 0 1))
                (substring docstring 1)))))

(cube--defpiece center-green [0.0 0.0 2.0]
  'cube-green nil nil nil nil nil
  "Center piece with green color.")

(cube--defpiece edge-green-orange [-2.0 0.0 2.0]
  'cube-green nil 'cube-orange nil nil nil
  "Edge piece with green and orange color.")

(cube--defpiece edge-green-red [2.0 0.0 2.0]
  'cube-green nil nil 'cube-red nil nil
  "Edge piece with green and red color.")

(cube--defpiece edge-green-white [0.0 2.0 2.0]
  'cube-green nil nil nil 'cube-white nil
  "Edge piece with green and white color.")

(cube--defpiece edge-green-yellow [0.0 -2.0 2.0]
  'cube-green nil nil nil nil 'cube-yellow
  "Edge piece with green and yellow color.")

(cube--defpiece corner-green-orange-white [-2.0 2.0 2.0]
  'cube-green nil 'cube-orange nil 'cube-white nil
  "Corner piece with green, orange and white color.")

(cube--defpiece corner-green-orange-yellow [-2.0 -2.0 2.0]
  'cube-green nil 'cube-orange nil nil 'cube-yellow
  "Corner piece with green, orange and yellow color.")

(cube--defpiece corner-green-red-white [2.0 2.0 2.0]
  'cube-green nil nil 'cube-red 'cube-white nil
  "Corner piece with green, red and white color.")

(cube--defpiece corner-green-red-yellow [2.0 -2.0 2.0]
  'cube-green nil nil 'cube-red nil 'cube-yellow
  "Corner piece with green, red and yellow color.")

(cube--defpiece center-blue [0.0 0.0 -2.0]
  nil 'cube-blue nil nil nil nil
  "Center piece with blue color.")

(cube--defpiece edge-blue-orange [-2.0 0.0 -2.0]
  nil 'cube-blue 'cube-orange nil nil nil
  "Edge piece with blue and orange color.")

(cube--defpiece edge-blue-red [2.0 0.0 -2.0]
  nil 'cube-blue nil 'cube-red nil nil
  "Edge piece with blue and red color.")

(cube--defpiece edge-blue-white [0.0 2.0 -2.0]
  nil 'cube-blue nil nil 'cube-white nil
  "Edge piece with blue and white color.")

(cube--defpiece edge-blue-yellow [0.0 -2.0 -2.0]
  nil 'cube-blue nil nil nil 'cube-yellow
  "Edge piece with blue and yellow color.")

(cube--defpiece corner-blue-orange-white [-2.0 2.0 -2.0]
  nil 'cube-blue 'cube-orange nil 'cube-white nil
  "Corner piece with blue, orange and white color.")

(cube--defpiece corner-blue-orange-yellow [-2.0 -2.0 -2.0]
  nil 'cube-blue 'cube-orange nil nil 'cube-yellow
  "Corner piece with blue, orange and yellow color.")

(cube--defpiece corner-blue-red-white [2.0 2.0 -2.0]
  nil 'cube-blue nil 'cube-red 'cube-white nil
  "Corner piece with blue, red and white color.")

(cube--defpiece corner-blue-red-yellow [2.0 -2.0 -2.0]
  nil 'cube-blue nil 'cube-red nil 'cube-yellow
  "Corner piece with blue, red and yellow color.")

(cube--defpiece center-orange [-2.0 0.0 0.0]
  nil nil 'cube-orange nil nil nil
  "Center piece with orange color.")

(cube--defpiece edge-orange-white [-2.0 2.0 0.0]
  nil nil 'cube-orange nil 'cube-white nil
  "Edge piece with blue and white color.")

(cube--defpiece edge-orange-yellow [-2.0 -2.0 0.0]
  nil nil 'cube-orange nil nil 'cube-yellow
  "Edge piece with orange and yellow color.")

(cube--defpiece center-red [2.0 0.0 0.0]
  nil nil nil 'cube-red nil nil
  "Center piece with red color.")

(cube--defpiece edge-red-white [2.0 2.0 0.0]
  nil nil nil 'cube-red 'cube-white nil
  "Edge piece with red and white color.")

(cube--defpiece edge-red-yellow [2.0 -2.0 0.0]
  nil nil nil 'cube-red nil 'cube-yellow
  "Edge piece with red and yellow color.")

(cube--defpiece center-white [0.0 2.0 0.0]
  nil nil nil nil 'cube-white nil
  "Center piece with white color.")

(cube--defpiece center-yellow [0.0 -2.0 0.0]
  nil nil nil nil nil 'cube-yellow
  "Center piece with yellow color.")

(defvar cube--framebuffer nil
  "Framebuffer for drawing cube.")

(defun cube--write-image ()
  "Write framebuffer to current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (cond
     ((display-graphic-p)
      (let ((rect-format (format
                          (concat "<rect width=\"%i\" height=\"%i\""
                                  " x=\"%%i\" y=\"%%i\" fill=\"%%s\"/>")
                          cube-scale cube-scale))
            (y 0))
        (insert
         (format
          (concat "<svg width=\"%i\" height=\"%i\" version=\"1.1\""
                  " xmlns=\"http://www.w3.org/2000/svg\""
                  " xmlns:xlink=\"http://www.w3.org/1999/xlink\">")
          (* (length (aref eagle-framebuffer 0)) cube-scale)
          (* (length eagle-framebuffer) cube-scale)))
        (mapc
         (lambda (l)
           (let ((x 0))
             (mapc
              (lambda (p)
                (when p
                  (insert (format rect-format x y (color-rgb-to-hex
                                                   (aref (aref p 0) 0)
                                                   (aref (aref p 0) 1)
                                                   (aref (aref p 0) 2)
                                                   2))))
                (setq x (+ x cube-scale)))
              l))
           (setq y (+ y cube-scale)))
         eagle-framebuffer)
        (insert "</svg>")
        (let ((svg (buffer-substring-no-properties (point-min)
                                                   (point-max))))
          (erase-buffer)
          (insert-image (create-image svg 'svg t)))))
     ((and (char-displayable-p ?▀)  ; UPPER HALF BLOCK
           (char-displayable-p ?▄)) ; LOWER HALF BLOCK
      (dotimes (i (ceiling (/ (length cube--framebuffer) 2.0)))
        (dotimes (j (length (aref cube--framebuffer 0)))
          (let* ((up (aref (aref cube--framebuffer (* i 2)) j))
                 (down (when (< (1+ (* i 2))
                                (length cube--framebuffer))
                         (aref (aref cube--framebuffer (1+ (* i 2)))
                               j)))
                 (up-color (when up
                             (color-rgb-to-hex
                              (aref (aref up 0) 0)
                              (aref (aref up 0) 1)
                              (aref (aref up 0) 2)
                              2)))
                 (down-color (when down
                               (color-rgb-to-hex
                                (aref (aref down 0) 0)
                                (aref (aref down 0) 1)
                                (aref (aref down 0) 2)
                                2))))
            (cond
             ((and (not up-color) (not down-color))
              (insert " "))
             ((and up-color (not down-color))
              (insert
               (propertize "▀" 'face (list :foreground up-color))))
             ((and (not up-color) down-color)
              (insert
               (propertize "▄" 'face (list :foreground down-color))))
             ((equal up-color down-color)
              (insert
               (propertize " " 'face (list :foreground up-color
                                           :background up-color))))
             (t
              (insert
               (propertize "▄" 'face (list :foreground down-color
                                           :background up-color)))))))
        (insert "\n")))
     (t
      (mapc
       (lambda (l)
         (mapc
          (lambda (p)
            (insert
             (if (not p)
                 "  "
               (let ((color (color-rgb-to-hex
                             (aref (aref p 0) 0)
                             (aref (aref p 0) 1)
                             (aref (aref p 0) 2)
                             2)))
                 (propertize "  " 'face (list :foreground color
                                              :background color))))))
          l)
         (insert "\n"))
       cube--framebuffer)))))

(defun cube-update-framebuffer ()
  "Synchronize current framebuffer size with the value `cube-resolution'."
  (setq cube--framebuffer (eagle-make-framebuffer (car cube-resolution)
                                                  (cdr cube-resolution))))

(defvar-local cube--camera-position nil
  "Camera position.")

(defvar-local cube--camera-up nil
  "Vector pointing upward from the camera.")

(defvar-local cube--view-matrix nil
  "View matrix.")

(eval-and-compile
  (defconst cube--pieces '(center-green
                           edge-green-orange
                           edge-green-red
                           edge-green-white
                           edge-green-yellow
                           corner-green-orange-white
                           corner-green-orange-yellow
                           corner-green-red-white
                           corner-green-red-yellow
                           center-blue
                           edge-blue-orange
                           edge-blue-red
                           edge-blue-white
                           edge-blue-yellow
                           corner-blue-orange-white
                           corner-blue-orange-yellow
                           corner-blue-red-white
                           corner-blue-red-yellow
                           center-orange
                           edge-orange-white
                           edge-orange-yellow
                           center-red
                           edge-red-white
                           edge-red-yellow
                           center-white
                           center-yellow)
    "All pieces of cube."))

(defun cube-render ()
  "Render cube."
  (interactive)
  (unless cube--framebuffer
    (cube-update-framebuffer))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (eagle-framebuffer cube--framebuffer)
         (projection (eagle-perspective (/ float-pi 4)
                                        (/ (float (car cube-resolution))
                                           (cdr cube-resolution))
                                        0.1 25.0))
         (eagle-geometry-shader (lambda ()
                                  (let ((a (eagle-get-vertex 0))
                                        (b (eagle-get-vertex 1))
                                        (c (eagle-get-vertex 2)))
                                    (prog1
                                        (list (list a b c))
                                      (eagle-pop-vertices 3)))))
         (eagle-fragment-shader (lambda (p)
                                  (vector (nth 4 p) (nth 5 p) (nth 6 p)
                                          1.0)))
         (eagle-cull-triangles 'cw))
    (eagle-clear-framebuffer)
    (dolist (piece cube--pieces)
      (let* ((rotation (symbol-value
                        (intern (format "cube--%S-piece-rotation" piece))))
             (eagle-vertex-shader
              (lambda (v)
                `[,@(append
                     (eagle-multiply
                      projection cube--view-matrix rotation
                      (vector (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
                     nil)
                  ,@(color-name-to-rgb (face-foreground (aref v 4)))])))
        (eagle-draw (symbol-value
                     (intern (format "cube--%S-piece" piece))))))
    (cube--write-image)))

(defun cube--update-view-matrix ()
  "Update `cube--view-matrix'."
  (setq cube--view-matrix
        (eagle-look-at (vector (aref cube--camera-position 0)
                               (aref cube--camera-position 1)
                               (aref cube--camera-position 2))
                       [0.0 0.0 0.0]
                       (vector (aref cube--camera-up 0)
                               (aref cube--camera-up 1)
                               (aref cube--camera-up 2)))))



(defun cube--rotate-camera (axis count &optional no-render)
  "Rotate camera around AXIS by COUNT right angles.

Render the cube unless NO-RENDER is non-nil."
  (let* ((matrix (eagle-rotate [[1.0 0.0 0.0 0.0]
                                [0.0 1.0 0.0 0.0]
                                [0.0 0.0 1.0 0.0]
                                [0.0 0.0 0.0 1.0]]
                               (* (/ float-pi 2) count)
                               axis))
         (camera (eagle-multiply matrix
                                 `[,@(append cube--camera-position nil)
                                   1.0]))
         (camera-up (eagle-multiply matrix
                                    `[,@(append cube--camera-up nil)
                                      1.0])))
    (setq cube--camera-position (vector (aref camera 0) (aref camera 1)
                                        (aref camera 2)))
    (setq cube--camera-up (vector (aref camera-up 0) (aref camera-up 1)
                                  (aref camera-up 2))))
  (cube--update-view-matrix)
  (unless no-render
    (cube-render)))

(defun cube-camera-rotate-right (&optional arg)
  "Rotate cube right.

ARG, interactively the prefix arg, when given, specifies how to many times
to rotate."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (cube--rotate-camera cube--camera-up
                       (/ (float (* cube-camera-rotation-amount arg)) 90)))

(defun cube-camera-rotate-left (&optional arg)
  "Rotate cube left.

ARG, interactively the prefix arg, when given, specifies how to many times
to rotate."
  (interactive "p")
  (cube--rotate-camera (eagle-subtract cube--camera-up)
                       (/ (float (* cube-camera-rotation-amount arg)) 90)))

(defun cube-camera-rotate-up (&optional arg)
  "Rotate cube up.

ARG, interactively the prefix arg, when given, specifies how to many times
to rotate."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (cube--rotate-camera (eagle-cross cube--camera-position cube--camera-up)
                       (/ (float (* cube-camera-rotation-amount arg)) 90)))

(defun cube-camera-rotate-down (&optional arg)
  "Rotate cube down.

ARG, interactively the prefix arg, when given, specifies how to many times
to rotate."
  (interactive "p")
  (cube--rotate-camera (eagle-cross cube--camera-up cube--camera-position)
                       (/ (float (* cube-camera-rotation-amount arg)) 90)))

(defun cube-camera-rotate-clockwise (&optional arg)
  "Rotate cube clockwise.

ARG, interactively the prefix arg, when given, specifies how to many times
to rotate."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (cube--rotate-camera cube--camera-position
                       (/ (float (* cube-camera-rotation-amount arg)) 90)))

(defun cube-camera-rotate-counter-clockwise (&optional arg)
  "Rotate cube counter-clockwise.

ARG, interactively the prefix arg, when given, specifies how to many times
to rotate."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (cube--rotate-camera (eagle-subtract cube--camera-position)
                       (/ (float (* cube-camera-rotation-amount arg)) 90)))

(defun cube-camera-rotate-show-back ()
  "Rotate the cube and show the back side."
  (interactive)
  (setq cube--camera-position (eagle-subtract cube--camera-position))
  (setq cube--camera-up (eagle-subtract cube--camera-up))
  (cube--update-view-matrix)
  (cube-render))

(defun cube-camera-reset-rotation (&optional no-update)
  "Reset to the rotation of cube.

If NO-UPDATE is non-nil, don't update cube."
  (interactive)
  (let ((camera (eagle-multiply [0.0 0.0 1.0] cube-camera-distance))
        (camera-up [0.0 1.0 0.0]))
    (setq cube--camera-position camera)
    (setq cube--camera-up camera-up)
    (cube--update-view-matrix))
  (unless no-update
    (cube-render)))

(defmacro cube--save-piece-rotations (&rest body)
  "Eval BODY and restore piece rotations."
  (declare (indent 0))
  `(let ,(let ((pairs '((cube--camera-position cube--camera-position)
                        (cube--camera-up cube--camera-up))))
           (dolist (piece cube--pieces)
             (let ((sym (intern (format "cube--%S-piece-rotation" piece))))
               (push `(,sym ,sym) pairs)))
           pairs)
     ,@body))

(defun cube--perform-rotation (face type count &optional no-render
                                    no-animate)
  "Do a rotation.

FACE should be a vector.  TYPE should be either `single', `double', `cube'
or `middle'.  COUNT should be a number, possibly negative for clockwise
rotation.  When NO-RENDER is non-nil, don't render cube.  When NO-ANIMATE
is non-nil, don't animate."
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-quit t))
    (unless (or no-render no-animate)
      (while-no-input
        (dotimes (i (1- cube-animation-steps))
          (cube--save-piece-rotations
            (let ((time (current-time)))
              (cube--perform-rotation
               face type
               (* (/ (+ (sin (- (* (/ float-pi cube-animation-steps)
                                   (1+ i))
                                (/ float-pi 2)))
                        1.0)
                     2)
                  count)
               nil t)
              (redisplay)
              (sleep-for
               (- (/ cube-animation-duration (1- cube-animation-steps))
                  (float-time (time-since time)))))))))
    (pcase type
      ('single
       (dolist (piece cube--pieces)
         (let* ((rotation-sym (intern (format "cube--%S-piece-rotation"
                                              piece)))
                (rotation (symbol-value rotation-sym))
                (direction (eagle-normalize
                            (eagle-multiply
                             rotation
                             (symbol-value
                              (intern (format "cube--%S-piece-position"
                                              piece)))))))
           (when (< (acos (eagle-dot (vector (aref direction 0)
                                             (aref direction 1)
                                             (aref direction 2))
                                     (eagle-normalize face)))
                    1)
             (set rotation-sym
                  (eagle-multiply
                   (eagle-rotate [[1.0 0.0 0.0 0.0]
                                  [0.0 1.0 0.0 0.0]
                                  [0.0 0.0 1.0 0.0]
                                  [0.0 0.0 0.0 1.0]]
                                 (* (/ float-pi 2) count) face)
                   rotation))))))
      ('double
       (cube--perform-rotation (eagle-subtract face) 'single (- count) t t)
       (cube--rotate-camera (eagle-subtract face) (- count) t))
      ('middle
       (cube--perform-rotation (eagle-subtract face) 'single (- count) t t)
       (cube--perform-rotation face 'single count t t)
       (cube--rotate-camera face count t))
      ('cube
       (cube--rotate-camera face count t)))
    (unless no-render
      (cube-render))))

(defun cube--get-face (face)
  "Get FACE relative to current camera location."
  (cl-labels ((closest-axis
               (vector)
               (eagle-normalize
                (cond ((and (> (abs (aref vector 0))
                               (abs (aref vector 1)))
                            (> (abs (aref vector 0))
                               (abs (aref vector 2))))
                       (vector (aref vector 0) 0.0 0.0))
                      ((and (> (abs (aref vector 1))
                               (abs (aref vector 0)))
                            (> (abs (aref vector 1))
                               (abs (aref vector 2))))
                       (vector 0.0 (aref vector 1) 0.0))
                      ((and (> (abs (aref vector 2))
                               (abs (aref vector 0)))
                            (> (abs (aref vector 2))
                               (abs (aref vector 1))))
                       (vector 0.0 0.0 (aref vector 2)))))))
    (pcase face
      ('front (closest-axis cube--camera-position))
      ('back (eagle-subtract (closest-axis cube--camera-position)))
      ('left (closest-axis (eagle-cross cube--camera-position
                                          cube--camera-up)))
      ('right (closest-axis (eagle-cross cube--camera-up
                                           cube--camera-position)))
      ('up (closest-axis cube--camera-up))
      ('down (eagle-subtract (closest-axis cube--camera-up))))))

(defun cube-compile-algorithm (algorithm)
  "Compile ALGORITHM.

ALGORITHM should be string like \"R D R' D'\".  This function is a nop in
case ALGORITHM already is compiled."
  (declare (pure t))
  (if (not (stringp algorithm))
      algorithm
    (let ((regexp (rx string-start
                      (group (or ?F ?U ?R ?L ?D ?B
                                 ?f ?u ?r ?l ?d ?b
                                 ?M ?E ?S ?x ?y ?z))
                      (group (or ?' (zero-or-more digit)))
                      string-end)))
      (mapcar
       (lambda (part)
         (if (not (string-match regexp part))
             (error "Syntax error in algorithm")
           (let ((count (match-string 2 part)))
             (setq count (if (string= count "")
                             1
                           (if (string= count "'")
                               -1
                             (string-to-number count))))

             `(,@(pcase (aref (match-string 1 part) 0)
                   (?F '(front single))
                   (?U '(up single))
                   (?R '(right single))
                   (?L '(left single))
                   (?D '(down single))
                   (?B '(back single))
                   (?f '(front double))
                   (?u '(up double))
                   (?r '(right double))
                   (?l '(left double))
                   (?d '(down double))
                   (?b '(back double))
                   (?M '(left middle))
                   (?E '(down middle))
                   (?S '(front middle))
                   (?x '(right cube))
                   (?y '(up cube))
                   (?z '(front cube))
                   (_ (error "Invaild move '%s'" (match-string 1 part))))
               ,count))))
       (split-string algorithm)))))

(defun cube-decompile-algorithm (algorithm)
  "Decompile compiled algorithm ALGORITHM.

ALGORITHM should be a compiled algorithm returned by
`cube-compile-algorithm'.  This function is a nop in case ALGORITHM isn't
compiled."
  (declare (pure t))
  (if (stringp algorithm)
      algorithm
    (mapconcat
     (lambda (move)
       (concat (string (pcase (cons (car move) (cadr move))
                         ('(front . single) ?F)
                         ('(up . single) ?U)
                         ('(right . single) ?R)
                         ('(left . single) ?L)
                         ('(down . single) ?D)
                         ('(back . single) ?B)
                         ('(front . double) ?f)
                         ('(up . double) ?u)
                         ('(right . double) ?r)
                         ('(left . double) ?l)
                         ('(down . double) ?d)
                         ('(back . double) ?b)
                         ('(left . middle) ?M)
                         ('(down . middle) ?E)
                         ('(front . middle) ?S)
                         ('(right . cube) ?x)
                         ('(up . cube) ?y)
                         ('(front . cube) ?z)
                         (_ (error "Invaild algorithm"))))
               (unless (eq (caddr move) 1)
                 (if (< (caddr move) 0)
                     "'"
                   (number-to-string move)))))
     algorithm " ")))

(defun cube-execute-algorithm (algorithm &optional no-animate no-render)
  "Execute ALGORITHM on cube.

When NO-ANIMATE is non-nil, don't animate.  When NO-RENDER is non-nil,
don't render the cube (this also disables animation)."
  (let ((gc-cons-threshold most-positive-fixnum))
    (when (stringp algorithm)
      (setq algorithm (cube-compile-algorithm algorithm)))
    (while algorithm
      (let ((move (pop algorithm)))
        (cube--perform-rotation (cube--get-face (car move)) (cadr move)
                                (caddr move) no-animate no-animate)
        (when (and (not no-render)
                   (not no-animate)
                   algorithm)
          (redisplay)
          (sleep-for cube-animation-break)))
      (when (and no-animate (not no-render))
        (cube-render)))))

(defun cube-reverse-algorithm (algorithm)
  "Return the reverse algorithm of ALGORITHM."
  (declare (pure t))
  (let ((compiled-p (not (stringp algorithm)))
        (reverse nil))
    (unless compiled-p
      (setq algorithm (cube-compile-algorithm algorithm)))
    (dolist (move algorithm)
      (push (list (car move) (cadr move) (- (caddr move))) reverse))
    (unless compiled-p
      (setq reverse (cube-decompile-algorithm reverse)))
    reverse))

(defmacro cube--define-move (name move docstring-move)
  "Define a command named `cube-do-'`NAME' performing move MOVE on cube.

Show DOCSTRING-MOVE in the docstring."
  `(defun ,(intern (format "cube-do-%S" name)) (arg)
     ,(format "Perform %S on cube.

If ARG is negative, rotate counter-clockwise that many times.  When
NO-RENDER is non-nil, don't render cube."
              docstring-move)
       (interactive (list (cond ((not current-prefix-arg) 1)
                                ((or (listp current-prefix-arg)
                                     (eq current-prefix-arg '-))
                                 -1)
                                (t current-prefix-arg))))
       (cube-execute-algorithm ,move)))

(cube--define-move F `((front single ,arg)) "F")
(cube--define-move B `((back single ,arg)) "B")
(cube--define-move U `((up single ,arg)) "U")
(cube--define-move D `((down single ,arg)) "D")
(cube--define-move L `((left single ,arg)) "L")
(cube--define-move R `((right single ,arg)) "R")
(cube--define-move f `((front double ,arg)) "f")
(cube--define-move b `((back double ,arg)) "b")
(cube--define-move u `((up double ,arg)) "u")
(cube--define-move d `((down double ,arg)) "d")
(cube--define-move l `((left double ,arg)) "l")
(cube--define-move r `((right double ,arg)) "r")
(cube--define-move M `((left middle ,arg)) "M")
(cube--define-move E `((down middle ,arg)) "E")
(cube--define-move S `((front middle ,arg)) "S")
(cube--define-move x `((right cube ,arg)) "x")
(cube--define-move y `((up cube ,arg)) "y")
(cube--define-move z `((front cube ,arg)) "z")

(defun cube-reset ()
  "Reset cube."
  (interactive)
  (dolist (piece cube--pieces)
    (let* ((rotation (intern (format "cube--%S-piece-rotation" piece))))
      (set rotation [[1.0 0.0 0.0 0.0]
                     [0.0 1.0 0.0 0.0]
                     [0.0 0.0 1.0 0.0]
                     [0.0 0.0 0.0 1.0]])))
  (cube-render))

(defun cube-scramble (&optional seed)
  "Scramble the cube.

Use SEED to calculate random moves.  When SEED is nil, get the random seed
and use it.

Interactively, ask for SEED when prefix arg is given."
  (interactive (list (when current-prefix-arg (read-string "Seed: "))))
  (let ((gc-cons-threshold most-positive-fixnum))
    (let ((move-count (% (abs (random seed)) 30))
          (candidates '(front back up down left right))
          (algorithm nil)
          (previous-move nil))
      (dotimes (_ move-count)
        (let ((face (nth (% (abs (random)) (length candidates))
                         candidates))
              (prev-face previous-move))
          (push `(,face single ,(- (% (abs (random)) 3) 1)) algorithm)
          (setq previous-move (list face))
          (setq candidates (mapcan (lambda (f)
                                     (if (eq f face)
                                         prev-face
                                       (list f)))
                                   candidates))))
      (cube-execute-algorithm (nreverse algorithm)
                              (not cube-animate-on-shuffle))
      (message "Done"))))

(defun cube--make-bookmark-record ()
  "Make a bookmark record for current cube state."
  `((state ,@(let ((rotations nil))
               (dolist (piece cube--pieces)
                 (let ((sym (intern (format "cube--%S-piece-rotation"
                                            piece))))
                   (push (cons sym (symbol-value sym)) rotations)))
               rotations)
           (cube--camera-position . ,cube--camera-position)
           (cube--camera-up . ,cube--camera-up))
    (handler . cube-bookmark-jump)))

;;;###autoload
(defun cube-bookmark-jump (bookmark)
  "Jump to saved cube state in BOOKMARK."
  (with-current-buffer (generate-new-buffer "*cube*")
    (cube-mode)
    (dolist (pair (alist-get 'state (bookmark-get-bookmark-record
                                     bookmark)))
      (set (car pair) (cdr pair)))
    (cube--update-view-matrix)
    (display-buffer (current-buffer))
    (cube-render)))

(defvar cube-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "f") #'cube-do-F)
    (define-key keymap (kbd "b") #'cube-do-B)
    (define-key keymap (kbd "l") #'cube-do-L)
    (define-key keymap (kbd "r") #'cube-do-R)
    (define-key keymap (kbd "u") #'cube-do-U)
    (define-key keymap (kbd "d") #'cube-do-D)
    (define-key keymap (kbd "F") #'cube-do-f)
    (define-key keymap (kbd "B") #'cube-do-b)
    (define-key keymap (kbd "L") #'cube-do-l)
    (define-key keymap (kbd "R") #'cube-do-r)
    (define-key keymap (kbd "U") #'cube-do-u)
    (define-key keymap (kbd "D") #'cube-do-d)
    (define-key keymap (kbd "m") #'cube-do-M)
    (define-key keymap (kbd "e") #'cube-do-E)
    (define-key keymap (kbd "s") #'cube-do-S)
    (define-key keymap (kbd "x") #'cube-do-x)
    (define-key keymap (kbd "y") #'cube-do-y)
    (define-key keymap (kbd "z") #'cube-do-z)
    (define-key keymap (kbd "O") #'cube-reset)
    (define-key keymap (kbd "P") #'cube-scramble)
    (define-key keymap (kbd "g") #'cube-render)
    (define-key keymap [up] #'cube-camera-rotate-up)
    (define-key keymap [down] #'cube-camera-rotate-down)
    (define-key keymap [left] #'cube-camera-rotate-left)
    (define-key keymap [right] #'cube-camera-rotate-right)
    (define-key keymap [M-up] #'cube-camera-rotate-clockwise)
    (define-key keymap [M-down] #'cube-camera-rotate-counter-clockwise)
    (define-key keymap (kbd "C-p") #'cube-camera-rotate-up)
    (define-key keymap (kbd "C-n") #'cube-camera-rotate-down)
    (define-key keymap (kbd "C-b") #'cube-camera-rotate-left)
    (define-key keymap (kbd "C-f") #'cube-camera-rotate-right)
    (define-key keymap (kbd "M-p") #'cube-camera-rotate-counter-clockwise)
    (define-key keymap (kbd "M-n") #'cube-camera-rotate-clockwise)
    (define-key keymap (kbd "B") #'cube-camera-rotate-show-back)
    (define-key keymap (kbd "SPC") #'cube-camera-reset-rotation)
    keymap)
  "Keymap for `cube-mode'.")

(define-derived-mode cube-mode special-mode "Cube"
  "Mode for solving Rubik's Cube."
  (cube-camera-reset-rotation t)
  (setq-local cursor-type nil)
  (setq-local buffer-undo-list t)
  (setq-local bookmark-make-record-function #'cube--make-bookmark-record))

;;;###autoload
(defun cube ()
  "Solve the Rubik's Cube."
  (interactive)
  (with-current-buffer (generate-new-buffer "*cube*")
    (cube-mode)
    (display-buffer (current-buffer))
    (cube-render)))

(provide 'cube)
;;; cube.el ends here
