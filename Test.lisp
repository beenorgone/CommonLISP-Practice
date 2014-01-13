(defun num->text (num)
       (append (groups-of-nine->text (num->groups-of-nine num))
	       measure-unit))

(setf measure-unit '(đơn vị))

