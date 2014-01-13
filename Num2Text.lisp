"(defun num->text [num] ...)"
"(num->text 1,034,567,890) -> 'một tỷ không trăm ...'"

"Solution:
Use function NUM->GROUPS-OF-THREE to separate NUM into groups of three (from the end of NUM). After that, write function GROUPS-OF-THREE->TEXT to convert the groups into text, interlaced by unit of the groups (đơn-vị, ngàn, triệu, tỷ, nghìn tỷ)
(num->groups-of-three 1234567890) -> (1 234 567 890)
Write GROUPS-OF-THREE->TEXT, a function that takes a group of three as input and read them as text.
Noted:
- Ignore 0s at the head of the first group.
- If a group of three is zero all, ignore both the group and its unit.
- If the third & the second of the group are zero, ignore both. 
- Syllables change: một - mốt, không - linh, mười - mươi, năm - lăm"

(defun num->text (num)
       (groups-of-three->text (num->groups-of-three num)))

(defun num->groups-of-three (num) ;(num->groups-of-three 1023405789) -> (1 23 405 789)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (num->groups-of-three (floor num 1000))
			(list (mod num 1000))))))

"Test cases"
(num->groups-of-three 0123456789) ;(123 456 789)
(num->groups-of-three 1234001040) ;(1 234 1 40)

;NUM->GROUPS-OF-THREE function remove 0s in the NUM's head automatically

(defun groups-of-three->text (nums)
       (append (first-group-of-three->text (first nums))
	       (list (add-unit (length nums)))
	       (rest-groups-of-three->text (rest nums))
	       measure-unit))

(defun rest-groups-of-three->text (nums)
       (cond ((zerop (length nums)) nil)
	     (t (append (group-of-three->text (first nums))
			(if (zerop (first nums)) nil ;If a group of three is zero, ignore both the group and its unit.
				   (list (add-unit (length nums))))
			(rest-groups-of-three->text (rest nums))))))

(setf units-table
      '((5 nghìn-tỷ)
	(4 tỷ)
	(3 triệu)
	(2 ngàn)
	(1 nil)))

(setf measure-unit '(đơn-vị)) ;replace đơn-vị with m2, m3, vnd, usd, ...

(defun add-unit (a)
       (second (assoc a units-table)))

"Test cases"
(add-unit 1)	;đơn-vị
(add-unit 2)	;ngàn
(add-unit 6)	;nil

(setf num->text-table
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

"Test cases"
(first-num->text 0)	;(KHÔNG TRĂM ĐƠN-VỊ)
(first-num->text 123)	;(MỘT TRĂM ĐƠN-VỊ)

(defun second-num->text (num)
       (if (zerop (mod num 100)) ;If the third & the second of the group are zero, ignore both.
	   nil
	   (third (assoc (mod (floor num 10) 10)
			 num->text-table))) )

"Test cases"
(second-num->text 0)	;NIL (ignored)
(second-num->text 203)	;(LINH)
(second-num->text 223)	;(HAI MƯƠI)
(second-num->text 210)	;(MƯỜI)

(defun third-num->text (num)
       (cond ((zerop (mod num 10)) nil)
	     ((or (equalp (mod num 100) 11)
		  (equalp (mod num 100) 1)) '(một))
	     ((equalp (mod num 100) 5) '(năm))
	     (t (list (fourth (assoc (mod num 10)
				     num->text-table))))))

"Test cases"
(third-num->text 0)	;NIL (ignored)
(third-num->text 200)	;NIL (ignored)
(third-num->text 234)	;(TƯ)
(third-num->text 205)	;(NĂM)
(third-num->text 235)	;(LĂM)
(third-num->text 911)	;(MỘT)
(third-num->text 901)	;(MỘT)
(third-num->text 921)	;(MỐT)

"Test cases"
;GROUP-OF-THREE->TEXT

(group-of-three->text 0)		;NIL (ignored)
(group-of-three->text 1)		;(KHÔNG TRĂM LINH MỘT)
(group-of-three->text 901)	;(CHÍN TRĂM LINH MỘT)
(group-of-three->text 911)	;(CHÍN TRĂM MƯỜI MỘT)
(group-of-three->text 921)	;(CHÍN TRĂM HAI MƯƠI MỐT)
(group-of-three->text 200)	;(HAI TRĂM)
(group-of-three->text 234)	;(HAI TRĂM BA MƯƠI TƯ)
(group-of-three->text 235)	;(HAI TRĂM BA MƯƠI LĂM)
(group-of-three->text 555)	;(NĂM TRĂM NĂM MƯƠI LĂM)
(group-of-three->text 205)	;(HAI TRĂM LINH NĂM)

;FIRST-GROUP-OF-THREE->TEXT

(first-group-of-three->text 5)		;(NĂM)
(first-group-of-three->text 11)		;(MƯỜI MỘT)
(first-group-of-three->text 15)		;(MƯỜI LĂM)

;GROUPS-OF-THREE->TEXT

(groups-of-three->text '(901 911 200 205))	;(CHÍN TRĂM LINH MỘT TỶ CHÍN TRĂM MƯỜI MỘT TRIỆU HAI TRĂM ...)
(groups-of-three->text '(901 000 200 205))	;(CHÍN TRĂM LINH MỘT TỶ HAI TRĂM NGÀN HAI TRĂM LINH NĂM ...)

;GROUPS-OF-THREE->TEXT function decrease 0s in the head of the NUM so we don't need to test FIRST-GROUP-OF-THREE->TEXT and GROUP-OF-THREE->TEXT functionS with 0

;NUM->TEXT

(num->text 1234567890)	;(MỘT TỶ HAI TRĂM BA MƯƠI BỐN TRIỆU NĂM TRĂM SÁU MƯƠI ...)
(num->text 9000002000)	;(CHÍN TỶ KHÔNG TRĂM LINH HAI NGÀN)
(num->text 01234605)	;(MỘT TRIỆU HAI TRĂM BA MƯƠI BỐN NGÀN SÁU TRĂM LINH LĂM ...)
(num->text 2011234567)	;(HAI TỶ KHÔNG TRĂM MƯỜI MỘT TRIỆU HAI TRĂM BA MƯƠI BỐN ...)
