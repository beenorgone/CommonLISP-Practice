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
	   (trios->words (group-of-nine->trios nums))
	   (append (first-group-of-nine->words (first nums))
		   '(tỷ)
		   "(trios->words (groups-of-nine->trios (rest nums))"
		   (rest-groups-of-nine->words (rest nums))
		   )))

(defun group-of-nine->trios (num)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (group-of-nine->trios (floor num 1000))
			(list (mod num 1000))))))

(defun rest-groups-of-nine->trios (nums)
       )