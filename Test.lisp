(defun num->word (num)
       (append (groups-of-nine->word (num->groups-of-nine num))
	       measure-unit))

(setf measure-unit '(đơn vị)) ;replace đơn-vị with m2, m3, vnd, usd, ...

(defun num->groups-of-nine (num) ;(num->groups-of-nine 1000234003) -> (1 234003)
       (cond ((zerop (floor num 1000000000)) (list num))
	     (t (append (num->groups-of-nine (floor num 1000000000))
			(list (mod num 1000000000))))))

"Test cases"
(num->groups-of-nine 1000234003) ;(1 234003)

(defun groups-of-nine->word (nums)
       (if (equalp 1 (length nums))
	   (trios->word (num->trios nums))
	   (append (first-group-of-nine->word (first nums))
		   '(tỷ)
		   "(trios->word (groups-of-nine->trios (rest nums))"
		   (rest-groups-of-nine->word (rest nums))
		   )))

(defun num->trios (num)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (num->trios (floor num 1000))
			(list (mod num 1000))))))

(defun trios->word (nums)
       (append (first-trio->word (first nums))
	       (list (add-unit (length nums)))
	       (rest-trios->word (rest nums))))

(setf unit-table
      '((3 triệu)
	(2 nghìn)
	(1 nil)))

(defun add-unit (a)
       (second (assoc a unit-table)))

(defun first-trio->word (num)
       (cond ((< num 10)
	      (list (second (assoc num num->word-table))))
	     ((< num 100) (append (second-digit->word num)
				  (third-digit->word)))
	     (t (trio->word num))))

(defun rest-trios->word (nums)
       (cond ((and (equalp (length nums) 1)
		   (zerop (first nums)))
	      nil)
	     (t (append (trio->word (first nums))
			(list (add-unit (length nums)))
			(rest-trios->word (rest nums))))))

(defun trio->word (num)
       (cond ((zerop num) '(không))
	     ))

(setf num->word-table
      '((0 không (linh)) ;linh v lẻ
	(1 một (mười) mốt)
	(2 hai (hai mươi) hai)
	(3 ba (ba mươi) ba)
	(4 bốn (bốn mươi) tư)
	(5 năm (năm mươi) lăm)
	(6 sáu (sáu mươi) sáu)
	(7 bảy (bảy mươi) bảy)
	(8 tám (tám mươi) tám)
	(9 chín (chín mươi) chín)))

(defun rest-groups-of-nine->trios (nums)
       )