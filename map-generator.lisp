;;;; overworld-interface.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.overworld)

(defun smooth-map (map)
  (let ((new-map (make-array (list +map-height+ +map-width+)
                             :element-type 'unsigned-byte :adjustable nil)))
    (dotimes (y +map-height+)
      (dotimes (x +map-width+)
        (cond
          ((and (>= (reduce #'+ (map-neighbors map x y)) 4) (= 1 (aref map y x)))
           (setf (aref new-map y x) 1))
          ((and (>= (reduce #'+ (map-neighbors map x y)) 5) (/= 1 (aref map y x)))
           (setf (aref new-map y x) 1))
          (t (setf (aref new-map y x) 0)))))
    new-map))

(defun map-neighbors (map x y)
  (list (map-peek map (+ x 1) y)
        (map-peek map (- x 1) y)
        (map-peek map x (+ y 1))
        (map-peek map x (- y 1))
        (map-peek map (+ x 1) (+ y 1))
        (map-peek map (+ x 1) (- y 1))
        (map-peek map (- x 1) (+ y 1))
        (map-peek map (- x 1) (- y 1))))

(defun map-peek (map x y)
  (cond
    ((>= x +map-width+) 0)
    ((< x 0) 0)
    ((>= y +map-height+) 0)
    ((< y 0) 0)
    (t (aref map y x))))
