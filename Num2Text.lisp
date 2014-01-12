"(defun num->text [num] ...)"
"(num->text 1,034,567,890) -> 'một tỷ không trăm ...'"

"Solution:
Use function NUM->GROUPS-OF-THREE to separate NUM into groups of three (from the end of NUM) interlaced by unit of the groups (đơn-vị, ngàn, triệu, tỷ, nghìn tỷ).
(num->groups-of-three 1234567890) -> (1 tỷ 234 triệu 567 ngàn 890 đơn vị)
Write GROUPS-OF-THREE->TEXT, a function that takes a group of three as input and read them as text.
Noted:
- Ignore 0s at the head of the first group.
- If a group of three is zero, ignore both the group and its unit.
- Syllables change: một - mốt, không - linh, mười - mươi, năm - lăm"

(defun num->text (num)
       (groups-of-three->text (num->groups-of-three num)))

(defun num->groups-of-three (num) ;(num->groups-of-three 1023405789) -> (1 23 405 789)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (num->groups-of-three
			  (floor num 1000))
			(list (mod num 1000))))))

(defun groups-of-three->text (nums)
       (append (first-group-of-three->text (first nums))
	       (list (add-unit (length nums)))
	       (rest-groups-of-three->text (rest nums))))

(defun rest-groups-of-three->text (nums)
       (cond ((zerop (length nums)) nil)
	     (t (append (group-of-three->text (first nums))
			(if (zerop (first nums))
				  nil ;If a group of three is zero, ignore both the group and its unit.
				  (list (add-unit (length nums))))
			(rest-groups-of-three->text (rest nums))))))

(setf units-table
      '((5 nghìn-tỷ)
	(4 tỷ)
	(3 triệu)
	(2 ngàn)
	(1 đơn-vị)))

(defun add-unit (a)
       (second (assoc a units-table)))

(setf num->text-table
      '((0 không (linh))
	(1 một (mười) mốt)
	(2 hai (hai mươi) hai)
	(3 ba (ba mươi) ba)
	(4 bốn (bốn mươi) bốn)
	(5 năm (năm mươi) lăm)
	(6 sáu (sáu mươi) sáu)
	(7 bảy (bảy mươi) bảy)
	(8 tám (tám mươi) tám)
	(9 chín (chín mươi) chín)))

(defun first-group-of-three->text (num)
       (cond ((< num 10) (list (second (assoc num
					      num->text-table))))
	     ((< num 100) (append (second-num->text num)
				  (third-num->text num)))
	     (t (group-of-three->text num))))

(defun group-of-three->text (num)
       (cond ((zerop num) nil) ;If a group of three is zero, ignore both the group and its unit.
	      (t (append (first-num->text num)
			 (second-num->text num)
			 (third-num->text num)))))

(defun first-num->text (num)
       (list (second (assoc (floor num 100)
			    num->text-table)) 'trăm))

;Ex:
(first-num->text 0)	;(KHÔNG TRĂM)
(first-num->text 123)	;(MỘT TRĂM)

(defun second-num->text (num)
       (third (assoc (mod (floor num 10) 10)
		     num->text-table)))

;Ex:
(second-num->text 0)	;(LINH)
(second-num->text 203)	;(LINH)
(second-num->text 223)	;(HAI MƯƠI)
(second-num->text 210)	;(MƯỜI)

(defun third-num->text (num)
       (cond ((zerop (mod num 10)) nil)
	     ((or (equalp (mod num 100) 11)
		  (equalp (mod num 100) 1)) '(một))
	     (t (list (fourth (assoc (mod num 10)
				     num->text-table))))))

;Ex:
(third-num->text 0)	;NIL (ignore)
(third-num->text 200)	;NIL (ignore)
(third-num->text 234)	;(BỐN)
(third-num->text 911)	;(MỘT)
(third-num->text 901)	;()

"Examples"

(num->text 1234567890)	;(MỘT TỶ HAI TRĂM BA MƯƠI BỐN TRIỆU NĂM TRĂM SÁU MƯƠI ...)
(num->text 9000002000)	;(CHÍN TỶ KHÔNG TRĂM LINH HAI NGÀN)
(num->text 01234605)	;(MỘT TRIỆU HAI TRĂM BA MƯƠI BỐN NGÀN SÁU TRĂM LINH LĂM ...)
(num->text 2011234567)	;(HAI TỶ KHÔNG TRĂM MƯỜI MỘT TRIỆU HAI TRĂM BA MƯƠI BỐN ...)
