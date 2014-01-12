"(defun num->text [num] ...)"
"(num->text 1,034,567,890) -> 'một tỷ không trăm ...'"

"Hướng giải:
Sử dụng hàm NUM->GROUPS-OF-THREE tách NUM thành các bộ 3 số (tính từ dưới lên) nối với nhau bởi đơn vị hàng ngàn, triệu, tỷ, nghìn tỷ.
1234567890 -> '1 tỷ 234 triệu 567 ngàn 890 đơn vị'
Viết hàm GROUPS-OF-THREE->TEXT để chuyển các bộ 3 số thành dạng text.
Lưu ý:
- Không đọc các số 0 ở đầu với bộ 3 đầu tiên.
- Nếu bộ 3 số bằng 0 thì bỏ qua không đọc cả bộ 3 số lẫn đơn vị
- Các biến âm đặc biệt: một - mốt, không - linh, mười - mươi, năm - lăm"

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
				  nil ;Nếu bộ 3 số bằng 0 thì bỏ qua không đọc cả bộ 3 số lẫn đơn vị
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
	     ((< num 100) (append (read-second-num num)
				  (read-third-num num)))
	     (t (group-of-three->text num))))

(defun group-of-three->text (num)
       (cond ((zerop num) nil) ;Nếu bộ 3 số bằng 0 thì bỏ qua không đọc cả bộ 3 số lẫn đơn vị
	      (t (append (read-first-num num)
			 (read-second-num num)
			 (read-third-num num)))))

(defun read-first-num (num)
       (list (second (assoc (floor num 100)
			    num->text-table)) 'trăm))

(defun read-second-num (num)
       (third (assoc (mod (floor num 10) 10)
		     num->text-table)))

(defun read-third-num (num)
       (cond ((zerop (mod num 10)) nil)
	     ((or (equalp (mod num 100) 11)
		  (equalp num 1)) '(một))
	     (t (list (fourth (assoc (mod num 10)
				     num->text-table))))))

"Examples"

(num->text 1234567890) ;(MỘT TỶ HAI TRĂM BA MƯƠI BỐN TRIỆU NĂM TRĂM SÁU MƯƠI ...)
(num->text 9000002000) ;(CHÍN TỶ KHÔNG TRĂM LINH HAI NGÀN)
(num->text 01234605)