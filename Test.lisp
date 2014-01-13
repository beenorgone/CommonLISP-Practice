(defun num->words (num)
       (append (groups-of-nine->words (num->groups-of-nine num))
	       measure-unit))

(setf measure-unit '(đơn vị)) ;replace đơn-vị with m2, m3, vnd, usd, ...

(defun num->groups-of-nine (num) ;(num->groups-of-nine 1000234003) -> (1 234003)
       (cond ((zerop (floor num 1000000000)) (list num))
	     (t (append (num->groups-of-nine (floor num 1000000000))
			(list (mod num 1000000000))))))

"Test cases"
(num->groups-of-nine 1000234003) ;(1 234003)

(defun groups-of-nine->words (nums)
       (if (equalp 1 (length nums))
	   (trios->words (num->trios nums))
	   (append (first-group-of-nine->words (first nums))
		   '(tỷ)
		   "(trios->words (groups-of-nine->trios (rest nums))"
		   (rest-groups-of-nine->words (rest nums))
		   )))

(defun num->trios (num)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (num->trios (floor num 1000))
			(list (mod num 1000))))))

(defun trios->words (nums)
       (append (first-trio->words (first nums))
	       (list (add-unit (length nums)))
	       (rest-trios->words (rest nums))))

(setf unit-table
      '((3 triệu)
	(2 nghìn)
	(1 nil)))

(defun add-unit (a)
       (second (assoc a unit-table)))

(defun)

(defun rest-groups-of-nine->trios (nums)
       )