(use-package :lisp-unit)

(define-test num->groups-of-nine-test
	     (assert-equal 2 (length (num->groups-of-nine 1000234456))))