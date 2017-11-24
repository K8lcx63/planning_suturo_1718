;; Foreign array system definition
;; Liam Healy 2012-03-23 19:06:29EDT foreign-array.asd
;; Time-stamp: <2015-03-14 10:27:22EDT foreign-array.asd>

;; Copyright 2012 Liam M. Healy
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+(or allegro ccl ecl lispworks sbcl)
(when (asdf:find-system :static-vectors nil)
  (pushnew :static-vectors *features*))

(asdf:defsystem #:foreign-array
  :name "Foreign-Array"
  :description "A library providing access to foreign (i.e., C) arrays."
  :author "Liam M. Healy"
  :license "GPL v3"
  :serial t
  :depends-on (#:antik-base :cffi #:trivial-garbage #+static-vectors static-vectors)
  :components
  ((:module foreign-array
    :components
    ((:file "types")
     (:file "complex-types" :depends-on ("types"))
     (:file "element-types" :depends-on ("types" "complex-types"))
     (:file "symbol-type")
     (:file "number-conversion"
      :depends-on ("complex-types" "symbol-type"))
     (:file "subclass" :depends-on ("element-types"))
     (:file "foreign-array" :depends-on ("types" "element-types"))
     (:file "vector-matrix" :depends-on ("element-types" "subclass"))
     (:file "methods" :depends-on ("foreign-array" "vector-matrix"))
     (:file "format-output")
     (:module tests
      :components
      ((:file "aref")
       (:file "lisp-unit-extension")
       (:file "compose")))))))



