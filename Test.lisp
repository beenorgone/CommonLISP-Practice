(defun num->text (num)
       (append (groups-of-nine->text (num->groups-of-nine num))
	       measure-unit))

(setf measure-unit '(đơn vị))

(defun num->groups-of-nine (num) ;(num->groups-of-nine 1000234003) -> (1 234003)
       (cond ((zerop (floor num 1000000000)) (list num))
	     (t (append (num->groups-of-nine (floor num 1000000000))
			(list (mod num 1000000000))))))