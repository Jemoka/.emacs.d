;;; eagle.el --- Eagle Advanced Graphics Library for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-17
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp extensions multimedia
;; Homepage: https://codeberg.org/akib/emacs-eagle

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

(require 'seq)
(require 'cl-lib)


;;; Maths

(defun eagle-vector (&rest components)
  "Make a vector containing COMPONENTS."
  (apply #'vector components))

(defun eagle-matrix (&rest rows)
  "Make a matrix.

ROWS should be the rows of the matrix."
  (apply #'vector rows))

(defun eagle--add-to-number (number b)
  "Add B to NUMBER."
  (if (vectorp b)
      (eagle--add-to-vector b number)
    (+ number b)))

(defun eagle--subtract-from-number (number b)
  "Subtract B from NUMBER."
  (if (vectorp b)
      (apply #'vector (mapcar (lambda (x) (- number x)) b))
    (- number b)))

(defun eagle--multiply-number-with (number b)
  "Multiply NUMBER with B."
  (if (vectorp b)
      (if (vectorp (aref b 0))
          (eagle--multiply-matrix-with b number)
        (eagle--multiply-vector-with b number))
    (* number b)))

(defun eagle--divide-number-by (number b)
  "Divide NUMBER by B."
  (if (vectorp b)
      (apply #'vector (mapcar (lambda (x) (/ number x)) b))
    (/ number b)))

(defun eagle--add-to-vector (vector b)
  "Add B to VECTOR."
  (if (vectorp b)
      (apply #'vector (seq-map-indexed (lambda (x i) (+ x (aref b i)))
                                       vector))
    (apply #'vector (mapcar (lambda (x) (+ x b)) vector))))

(defun eagle--subtract-from-vector (vector b)
  "Subtract B from VECTOR."
  (if (vectorp b)
      (apply #'vector (seq-map-indexed (lambda (x i) (- x (aref b i)))
                                     vector))
    (apply #'vector (mapcar (lambda (x) (- x b)) vector))))

(defun eagle--multiply-vector-with (vector b)
  "Multiply VECTOR with B."
  (apply #'vector (mapcar (lambda (x) (* x b)) vector)))

(defun eagle--divide-vector-by (vector b)
  "Multiply VECTOR with B."
  (apply #'vector (mapcar (lambda (x) (/ x b)) vector)))

(defun eagle--add-to-matrix (matrix b)
  "Add B to MATRIX."
  (apply #'vector
         (let ((rows nil))
           (dotimes (i (length matrix))
             (push (apply #'vector
                          (seq-map-indexed
                           (lambda (x j) (+ x (aref (aref b i) j)))
                           (aref matrix i)))
                   rows))
           (nreverse rows))))

(defun eagle--subtract-from-matrix (matrix b)
  "Subtract B from MATRIX."
  (apply #'vector
         (let ((rows nil))
           (dotimes (i (length matrix))
             (push (apply #'vector
                          (seq-map-indexed
                           (lambda (x j) (- x (aref (aref b i) j)))
                           (aref matrix i)))
                   rows))
           (nreverse rows))))

(defun eagle--multiply-matrix-with (matrix b)
  "Add B to MATRIX."
  (apply
   #'vector
   (let ((b-vector-p (not (vectorp (aref b 0))))
         (rows nil))
     (dotimes (i (length matrix))
       (let ((cols nil))
         (dotimes (j (if b-vector-p 1 (length (aref b 0))))
           (push (apply #'+ (seq-map-indexed
                             (if b-vector-p
                                 (lambda (x k) (* x (aref b k)))
                               (lambda (x k) (* x (aref (aref b k) j))))
                             (aref matrix i)))
                 cols))
         (push (if b-vector-p
                   (car cols)
                 (apply #'vector (nreverse cols)))
               rows)))
     (nreverse rows))))

(defun eagle-add (a &rest b)
  "Add A and B."
  (if (zerop (length b))
      a
    (setq b (if (eq (length b) 1) (car b) (apply #'eagle-add b)))
    (if (vectorp a)
        (if (vectorp (aref a 0))
            (eagle--add-to-matrix a b)
          (eagle--add-to-vector a b))
      (eagle--add-to-number a b))))

(defun eagle-subtract (a &rest b)
  "Subtract B from A."
  (if (not b)
      (eagle-subtract 0 a)
    (dolist (c b a)
      (setq a (if (vectorp a)
                  (if (vectorp (aref a 0))
                      (eagle--subtract-from-matrix a c)
                    (eagle--subtract-from-vector a c))
                (eagle--subtract-from-number a c))))))

(defun eagle-multiply (a &rest b)
  "Multiply A with B."
  (if (zerop (length b))
      a
    (setq b (if (eq (length b) 1) (car b) (apply #'eagle-multiply b)))
    (if (vectorp a)
        (if (vectorp (aref a 0))
            (eagle--multiply-matrix-with a b)
          (eagle--multiply-vector-with a b))
      (eagle--multiply-number-with a b))))

(defun eagle-divide (a &rest b)
  "Divide A by B."
  (if (not b)
      (eagle-divide 1 a)
    (dolist (c b a)
      (setq a (if (vectorp a)
                  (eagle--divide-vector-by a c)
                (eagle--divide-number-by a c))))))

(defun eagle-dot (a b)
  "Return the dot product of vector A and B."
  (apply #'+ (seq-map-indexed (lambda (x i) (* x (aref b i))) a)))

(defun eagle-cross (a b)
  "Return the cross product of A and B."
  (vector (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
          (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
          (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0)))))

(defun eagle-magnitude (a)
  "Return the magnitude of vector A."
  (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) a))))

(defun eagle-normalize (a)
  "Normalize vector A."
  (eagle-divide a (eagle-magnitude a)))

(defun eagle-translate (matrix position)
  "Make a translate matrix based on MATRIX.

The returned matrix translates a vector to POSITION."
  (let* ((col-0 (vector (aref (aref matrix 0) 0) (aref (aref matrix 1) 0)
                        (aref (aref matrix 2) 0) (aref (aref matrix 3) 0)))
         (col-1 (vector (aref (aref matrix 0) 1) (aref (aref matrix 1) 1)
                        (aref (aref matrix 2) 1) (aref (aref matrix 3) 1)))
         (col-2 (vector (aref (aref matrix 0) 2) (aref (aref matrix 1) 2)
                        (aref (aref matrix 2) 2) (aref (aref matrix 3) 2)))
         (col-3 (vector (aref (aref matrix 0) 3) (aref (aref matrix 1) 3)
                        (aref (aref matrix 2) 3) (aref (aref matrix 3) 3)))
         (col-3 (eagle-add (eagle-multiply col-0 (aref position 0))
                           (eagle-multiply col-1 (aref position 1))
                           (eagle-multiply col-2 (aref position 2))
                           col-3)))
    (vector
     (vector (aref col-0 0) (aref col-1 0) (aref col-2 0) (aref col-3 0))
     (vector (aref col-0 1) (aref col-1 1) (aref col-2 1) (aref col-3 1))
     (vector (aref col-0 2) (aref col-1 2) (aref col-2 2) (aref col-3 2))
     (vector (aref col-0 3) (aref col-1 3) (aref col-2 3)
             (aref col-3 3)))))

(defun eagle-scale (matrix scale)
  "Make scaling matrix based on MATRIX that scales a vector by SCALE."
  (let ((col-0 (eagle-multiply (vector (aref (aref matrix 0) 0)
                                       (aref (aref matrix 1) 0)
                                       (aref (aref matrix 2) 0)
                                       (aref (aref matrix 3) 0))
                               (aref scale 0)))
        (col-1 (eagle-multiply (vector (aref (aref matrix 0) 1)
                                       (aref (aref matrix 1) 1)
                                       (aref (aref matrix 2) 1)
                                       (aref (aref matrix 3) 1))
                               (aref scale 1)))
        (col-2 (eagle-multiply (vector (aref (aref matrix 0) 2)
                                       (aref (aref matrix 1) 2)
                                       (aref (aref matrix 2) 2)
                                       (aref (aref matrix 3) 2))
                               (aref scale 2)))
        (col-3 (vector (aref (aref matrix 0) 3) (aref (aref matrix 1) 3)
                       (aref (aref matrix 2) 3) (aref (aref matrix 3) 3))))
    (vector
     (vector (aref col-0 0) (aref col-1 0) (aref col-2 0) (aref col-3 0))
     (vector (aref col-0 1) (aref col-1 1) (aref col-2 1) (aref col-3 1))
     (vector (aref col-0 2) (aref col-1 2) (aref col-2 2) (aref col-3 2))
     (vector (aref col-0 3) (aref col-1 3) (aref col-2 3)
             (aref col-3 3)))))

(defun eagle-rotate (matrix angle axis)
  "Make a rotation matrix based on MATRIX.

The returned matrix rotates a vector by ANGLE radians around AXIS."
  (setq axis (eagle-normalize axis))
  (let* ((col-0 (vector (aref (aref matrix 0) 0) (aref (aref matrix 1) 0)
                        (aref (aref matrix 2) 0) (aref (aref matrix 3) 0)))
         (col-1 (vector (aref (aref matrix 0) 1) (aref (aref matrix 1) 1)
                        (aref (aref matrix 2) 1) (aref (aref matrix 3) 1)))
         (col-2 (vector (aref (aref matrix 0) 2) (aref (aref matrix 1) 2)
                        (aref (aref matrix 2) 2) (aref (aref matrix 3) 2)))
         (col-3 (vector (aref (aref matrix 0) 3) (aref (aref matrix 1) 3)
                        (aref (aref matrix 2) 3) (aref (aref matrix 3) 3)))
         (sin (sin angle))
         (cos (cos angle))
         (tmp (eagle-multiply axis (- 1 cos)))
         (n-col-0
          (eagle-add
           (eagle-multiply col-0 (+ cos (* (aref tmp 0) (aref axis 0))))
           (eagle-multiply col-1 (+ (* (aref tmp 0) (aref axis 1))
                                    (* sin (aref axis 2))))
           (eagle-multiply col-2 (- (* (aref tmp 0) (aref axis 2))
                                    (* sin (aref axis 1))))))
         (n-col-1
          (eagle-add
           (eagle-multiply col-0 (- (* (aref tmp 1) (aref axis 0))
                                    (* sin (aref axis 2))))
           (eagle-multiply col-1 (+ cos (* (aref tmp 1) (aref axis 1))))
           (eagle-multiply col-2 (+ (* (aref tmp 1) (aref axis 2))
                                    (* sin (aref axis 0))))))
         (n-col-2
          (eagle-add
           (eagle-multiply col-0 (+ (* (aref tmp 2) (aref axis 0))
                                    (* sin (aref axis 1))))
           (eagle-multiply col-1 (- (* (aref tmp 2) (aref axis 1))
                                    (* sin (aref axis 0))))
           (eagle-multiply col-2 (+ cos (* (aref tmp 2) (aref axis 2)))))))
    (vector
     (vector (aref n-col-0 0) (aref n-col-1 0) (aref n-col-2 0)
             (aref col-3 0))
     (vector (aref n-col-0 1) (aref n-col-1 1) (aref n-col-2 1)
             (aref col-3 1))
     (vector (aref n-col-0 2) (aref n-col-1 2) (aref n-col-2 2)
             (aref col-3 2))
     (vector (aref n-col-0 3) (aref n-col-1 3) (aref n-col-2 3)
             (aref col-3 3)))))

(defun eagle-look-at (eye-position target up)
  "Look at TARGET from EYE-POSITION while keep UP upwards."
  (let* ((d (eagle-normalize (eagle-subtract target eye-position)))
         (r (eagle-normalize (eagle-cross d up)))
         (u (eagle-cross r d)))
    `[[,(aref r 0) ,(aref r 1) ,(aref r 2) ,(- (eagle-dot r eye-position))]
      [,(aref u 0) ,(aref u 1) ,(aref u 2) ,(- (eagle-dot u eye-position))]
      [,(- (aref d 0)) ,(-  (aref d 1)) ,(- (aref d 2))
       ,(eagle-dot d eye-position)]
      [0 0 0 1]]))

(defun eagle-perspective (field-of-view aspect-ratio z-min z-max)
  "Make a perspective projection matrix.

The field of view of the returned projection matrix is FIELD-OF-VIEW.  It
has an aspect ratio of ASPECT-RATIO and it clips any point with Z value
less than Z-MIN or more than Z-MAX."
  (let ((half-tan (tan (/ (float field-of-view) 2))))
    `[[,(/ 1 (* aspect-ratio half-tan)) 0 0 0]
      [0 ,(/ 1 half-tan) 0 0]
      [0 0 ,(- (/ (+ z-min z-max) (- z-max z-min)))
         ,(/ (* z-min z-max 2) (- z-min z-max))]
      [0 0 -1 0]]))

(defun eagle-orthographic (left right bottom top z-min z-max)
  "Make a orthographic projection matrix.

Any point with X value less than LEFT and more than RIGHT, or Y value less
than BOTTOM and more than TOP, or Z value less than Z-MIN or more than
Z-MAX is clipped."
  `[[,(/ 2 (- right left)) 0 0 ,(/ (+ left right) (- left right))]
    [0 ,(/ 2 (- top bottom)) 0 ,(/ (+ bottom top) (- bottom top))]
    [0 0 ,(/ 2 (- z-min z-max)) ,(/ (+ z-min z-max) (- z-min z-max))]
    [0 0 0 1]])


;;; Rasterizer

(defvar eagle-framebuffer nil
  "Framebuffer to use.")

(defun eagle-make-framebuffer (width height)
  "Make a framebuffer of WIDTH*HEIGHT transparent pixels."
  (apply #'vector (mapcar (lambda (_) (make-vector width nil))
                          (number-sequence 1 height))))

(defun eagle-clear-framebuffer (&optional color)
  "Clear current framebuffer with COLOR.

If COLOR is nil, framebuffer is made transparent."
  (dotimes (i (length eagle-framebuffer))
    (dotimes (j (length (aref eagle-framebuffer i)))
      (aset (aref eagle-framebuffer i) j color))))

(defvar eagle--current-vertices nil
  "Vector of current vertices being drawn.")

(defvar eagle-fragment-shader nil
  "Fragment shader to use.")

(defun eagle--barycentric (p a b c)
  "Return barycentric coordinate for P inside A B C triangle."
  (let* ((v-a-x (- (car b) (car a)))
         (v-a-y (- (cdr b) (cdr a)))
         (v-b-x (- (car c) (car a)))
         (v-b-y (- (cdr c) (cdr a)))
         (v-c-x (- (car p) (car a)))
         (v-c-y (- (cdr p) (cdr a)))
         (div (float (- (* v-a-x v-b-y) (* v-b-x v-a-y))))
         (v (/ (- (* v-c-x v-b-y) (* v-b-x v-c-y)) div))
         (w (/ (- (* v-a-x v-c-y) (* v-c-x v-a-y)) div))
         (u (- 1 v w)))
    (when (or (isnan u) (isnan v) (isnan w))
      (let ((one-third (/ (float 1) 3)))
        (setq u one-third)
        (setq v one-third)
        (setq w one-third)))
    (vector u v w)))

(defun eagle--ndc-to-pixels (point)
  "Convert normalized device coordinate of POINT to pixel positions."
  (let ((w (aref point 3)))
    (vector (round (* (/ (+ (/ (aref point 0) w) 1) 2)
                      (length (aref eagle-framebuffer 0))))
            (round (* (/ (- 1 (/ (aref point 1) w)) 2)
                      (length eagle-framebuffer))))))

(defvar eagle-depth-test-predicate #'<
  "Predicate for depth testing.

The value should be a function taking two arguments, new depth and old
depth.  A fragment is rendered only and only if this function returns t.
Depth testing is disabled when this is nil.")

(defun eagle--draw-point (a)
  "Draw a point at A."
  (when (and (< -1 (car a) (length (aref eagle-framebuffer 0)))
             (< -1 (cdr a) (length eagle-framebuffer)))
    (let* ((barycentric (when (eq (length eagle--current-vertices) 3)
                          (eagle--barycentric
                           a (car (aref eagle--current-vertices 0))
                           (car (aref eagle--current-vertices 1))
                           (car (aref eagle--current-vertices 2)))))
           (ratio (when (eq (length eagle--current-vertices) 2)
                    (cond
                     ((and
                       (eq (aref (cdr (aref eagle--current-vertices 0)) 0)
                           (aref (cdr (aref eagle--current-vertices 1)) 0))
                       (eq (aref (cdr (aref eagle--current-vertices 0)) 1)
                           (aref (cdr (aref eagle--current-vertices 1))
                                 1)))
                      0.5)
                     ((eq (aref (cdr (aref eagle--current-vertices 0)) 0)
                          (aref (cdr (aref eagle--current-vertices 1)) 0))
                      (abs
                       (/ (float
                           (- (cdr a)
                              (aref (cdr (aref eagle--current-vertices 0))
                                    1)))
                          (- (aref (cdr (aref eagle--current-vertices 0))
                                   1)
                             (aref (cdr (aref eagle--current-vertices 1))
                                   1)))))
                     (t
                      (abs
                       (/
                        (float
                         (- (car a)
                            (aref (cdr (aref eagle--current-vertices 0))
                                  0)))
                        (- (aref (cdr (aref eagle--current-vertices 0))
                                 0)
                           (aref (cdr (aref eagle--current-vertices 1))
                                 0))))))))
           (interpolate
            (pcase (length eagle--current-vertices)
              (1
               (lambda (n)
                 (aref (cdr (aref eagle--current-vertices 0)) n)))
              (2
               (lambda (n)
                 (+ (* (aref (cdr (aref eagle--current-vertices 0)) n)
                       (- 1 ratio))
                    (* (aref (cdr (aref eagle--current-vertices 1)) n)
                       ratio))))
              (_
               (lambda (n)
                 (+ (* (aref (cdr (aref eagle--current-vertices 0)) n)
                       (aref barycentric 0))
                    (* (aref (cdr (aref eagle--current-vertices 1)) n)
                       (aref barycentric 1))
                    (* (aref (cdr (aref eagle--current-vertices 2)) n)
                       (aref barycentric 2)))))))
           (w (funcall interpolate 3))
           (z (funcall interpolate 2))
           (depth (/ z w)))
      (let ((current-pixel (when eagle-depth-test-predicate
                             (aref (aref eagle-framebuffer
                                         (- (1- (length eagle-framebuffer))
                                            (cdr a)))
                                   (car a)))))
        (when (and (<= -1.0 depth 1.0)
                   (or (not eagle-depth-test-predicate)
                       (not current-pixel)
                       (funcall eagle-depth-test-predicate depth
                                (aref current-pixel 1))))
          (let ((color
                 (funcall
                  eagle-fragment-shader
                  (let ((elements nil))
                    (dotimes (i (length
                                 (cdr (aref eagle--current-vertices 0))))
                      (push (cond ((eq i 2) z)
                                  ((eq i 3) w)
                                  (t (funcall interpolate i)))
                            elements))
                    (nreverse elements)))))
            (when color
              (aset (aref eagle-framebuffer
                          (- (1- (length eagle-framebuffer)) (cdr a)))
                    (car a) (vector color depth)))))))))

(defun eagle--bresenham-calc-points (a b callback)
  "Calculate all points draw to draw line from A to B.

Call CALLBACK with the position of each point."
  (when (> (car a) (car b))
    (let ((tmp a))
      (setq a b)
      (setq b tmp)))
  (let* ((swap-x-y (< (abs (- (car a) (car b)))
                      (abs (- (cdr a) (cdr b)))))
         (dx (abs (if swap-x-y
                      (- (cdr a) (cdr b))
                    (- (car a) (car b)))))
         (dy (abs (if swap-x-y
                      (- (car a) (car b))
                    (- (cdr a) (cdr b)))))
         (diff (- (* 2 dy) dx))
         (x (if swap-x-y (cdr a) (car a)))
         (y (if swap-x-y (car a) (cdr a)))
         (x-step (if (< x (if swap-x-y (cdr b) (car b))) 1 -1))
         (y-step (if (< y (if swap-x-y (car b) (cdr b))) 1 -1)))
    (dotimes (_ (1+ dx))
      (funcall callback (if swap-x-y (cons y x) (cons x y)))
      (when (> diff 0)
        (setq y (+ y y-step))
        (setq diff (- diff (* 2 dx))))
      (setq x (+ x x-step))
      (setq diff (+ diff (* 2 dy))))))

(defvar eagle-draw-method 'fill
  "How to draw lines and triangles.

When value is `fill', draw lines and fill triangles.  When value is `line',
draw lines but don't fill the inside of triangles.  When nil, draw only
point and treat lines and triangles as two and three separate point
respectively.")

(defun eagle--draw-line (a b)
  "Draw a line from A to B on FRAMEBUFFER."
  (if eagle-draw-method
      (eagle--bresenham-calc-points a b #'eagle--draw-point)
    (eagle--draw-point a)
    (eagle--draw-point b)))

(defun eagle--draw-triangle (a b c)
  "Draw a triangle on FRAMEBUFFER.

A, B and C should be vectors and the winding order should be
counter-clockwise."
  (pcase eagle-draw-method
    ('nil
     (eagle--draw-point a)
     (eagle--draw-point b)
     (eagle--draw-point c))
    ('line
     (eagle--draw-line a b)
     (eagle--draw-line b c)
     (eagle--draw-line c a))
    ('fill
     (unless (zerop (- (* (- (car b) (car a)) (- (cdr c) (cdr a)))
                       (* (- (cdr b) (cdr a)) (- (car c) (car a)))))
       (let* ((sorted-points (sort `[,a ,b ,c] (lambda (lhs rhs)
                                                 (< (cdr lhs) (cdr rhs)))))
              (bottom-most (aref sorted-points 0))
              (top-most (aref sorted-points 2))
              (sorted-points (sort `[,(aref sorted-points 0)
                                     ,(aref sorted-points 1)]
                                   #'car-less-than-car))
              (left-bottom (aref sorted-points 0))
              (right-bottom (aref sorted-points 1))
              (x (car top-most))
              (y (cdr top-most))
              (threshold (lambda (point-a point-b)
                           (if (or (< (cdr point-a) (cdr point-b))
                                   (and (equal (cdr point-a) (cdr point-b))
                                        (< (car point-b) (car point-a))))
                               0
                             1)))
              (ab-threshold (funcall threshold a b))
              (bc-threshold (funcall threshold b c))
              (ca-threshold (funcall threshold c a)))
         (cl-labels ((point-position
                      (p point-a point-b)
                      (- (* (- (car point-b) (car point-a))
                            (- (cdr p) (cdr point-a)))
                         (* (- (cdr point-b) (cdr point-a))
                            (- (car p) (car point-a)))))
                     (inside-p (p) (and (>= (point-position p a b) 0)
                                        (>= (point-position p b c) 0)
                                        (>= (point-position p c a) 0)))
                     (draw-maybe
                      (point)
                      (when (and (>= (point-position point a b)
                                     ab-threshold)
                                 (>= (point-position point b c)
                                     bc-threshold)
                                 (>= (point-position point c a)
                                     ca-threshold))
                        (eagle--draw-point point)
                        t)))
           (while (>= y (cdr bottom-most))
             ;; XXX I think something is wrong with the code to bring x
             ;; into triangle.  Swapping these two should not make any
             ;; different, but it makes.
             (while (> 0 (if (>= y (cdr right-bottom))
                             (point-position (cons x y) right-bottom
                                             top-most)
                           (point-position (cons x y) bottom-most
                                           right-bottom)))
               (setq x (1- x)))
             (while (> 0 (if (>= y (cdr left-bottom))
                             (point-position (cons x y) top-most
                                             left-bottom)
                           (point-position (cons x y) left-bottom
                                           bottom-most)))
               (setq x (1+ x)))
             (let ((current-x x))
               (while (inside-p (cons current-x y))
                 (draw-maybe (cons current-x y))
                 (setq current-x (1+ current-x))))
             (while (inside-p (cons (1- x) y))
               (setq x (1- x))
               (draw-maybe (cons x y)))
             (setq y (1- y)))))))))

(defun eagle--convert-ndc-to-pixel (point)
  "Convert normalized device coordinates of POINT to pixel positions."
  (let ((w (aref point 3)))
    (cons (floor (* (/ (+ (/ (aref point 0) w) 1) 2)
                    (1- (length (aref eagle-framebuffer 0)))))
          (floor (* (/ (+ (/ (aref point 1) w) 1) 2)
                    (1- (length eagle-framebuffer)))))))


;;; Geometry

(defvar eagle-geometry-shader nil
  "Geometry shader to use.")

(defvar eagle-vertex-shader nil
  "Vertex shader to use.")

(defvar eagle--vertices nil
  "Input vertices.")

(defvar eagle--next-vertex nil
  "Index of next vertex in `eagle--vertices'.")

(defun eagle-pop-vertices (count)
  "Pop COUNT vertices from remaining vertices."
  (setq eagle--next-vertex (+ eagle--next-vertex count)))

(defun eagle-get-vertex (n)
  "Return the Nth remaining vertex."
  (let* ((i (+ eagle--next-vertex n))
         (vertex (aref eagle--vertices i)))
    (unless (listp vertex)
      (setq vertex (list (funcall eagle-vertex-shader vertex)))
      (aset eagle--vertices i vertex))
    (car vertex)))

(defvar eagle-cull-triangles nil
  "Which triangles to cull.

When set to `cw', triangles with clockwise winding order are culled.  When
set to `ccw', triangles with counter-clockwise winding order are culled.
When nil, don't cull triangles.

This is effective even when triangles are not filled.")

(defun eagle--draw-shape (points)
  "Draw a shape using POINTS.

POINTS should a list of points.  If POINTS is empty, draw nothing.  If
POINTS has only one point, draw a point there.  If POINTS has two points,
draw a line between them, otherwise draw a triangle using POINTS."
  (pcase (length points)
    (0
     nil)
    (1
     (let* ((pixel-coord (eagle--convert-ndc-to-pixel (car points)))
            (eagle--current-vertices '((,pixel-coord . ,(car points)))))
       (eagle--draw-point pixel-coord)))
    (2
     (let* ((positions (mapcar #'eagle--convert-ndc-to-pixel points))
            (eagle--current-vertices
             (apply #'vector (seq-mapn #'cons positions points))))
       (eagle--draw-line (car positions) (cadr positions))))
    (_
     (let ((clockwise-winding-order-p
            (let* ((a (vector (/ (aref (car points) 0)
                                 (aref (car points) 3))
                              (/ (aref (car points) 1)
                                 (aref (car points) 3))
                              0.0))
                   (b (vector (/ (aref (cadr points) 0)
                                 (aref (cadr points) 3))
                              (/ (aref (cadr points) 1)
                                 (aref (cadr points) 3))
                              0.0))
                   (c (vector (/ (aref (caddr points) 0)
                                 (aref (caddr points) 3))
                              (/ (aref (caddr points) 1)
                                 (aref (caddr points) 3))
                              0.0)))
              (< (aref (eagle-cross (eagle-subtract b a)
                                     (eagle-subtract c a))
                       2)
                 0))))
       (when (or (not eagle-cull-triangles)
                 (if (eq eagle-cull-triangles 'cw)
                     (not clockwise-winding-order-p)
                   clockwise-winding-order-p))
         (let* ((positions (mapcar #'eagle--convert-ndc-to-pixel points))
                (eagle--current-vertices
                 (apply #'vector (seq-mapn #'cons positions points))))
           (apply #'eagle--draw-triangle (car positions)
                  (if clockwise-winding-order-p
                      (list (caddr positions) (cadr positions))
                    (list (cadr positions) (caddr positions))))))))))

(defun eagle-draw (points)
  "Draw POINTS."
  (let ((eagle--vertices (copy-sequence points))
        (eagle--next-vertex 0)
        (count (length points)))
    (while (< eagle--next-vertex count)
      (mapc #'eagle--draw-shape (funcall eagle-geometry-shader)))))

(provide 'eagle)
;;; eagle.el ends here
