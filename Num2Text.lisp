"(defun num->text [num] ...)"
"(num->text 1.034.567.890) -> 'một tỷ không trăm ...'"

"Hướng giải:
Sử dụng hàm NUM-2-3NUMS-GROUPS tách NUM thành các bộ 3 số (tính từ dưới lên) nối với nhau bởi đơn vị hàng ngàn, triệu, tỷ, nghìn tỷ.
1234567890 -> '1 tỷ 234 triệu 567 ngàn 890 đơn vị'
Viết hàm 3NUMS-GROUPS-2-TEXT để chuyển các bộ 3 số thành dạng text. Lưu ý: không đọc các số 0 ở đầu với bộ 3 đầu tiên."

(defun num-to-text (num)
       (3nums-groups-2-text (num-2-3nums-groups num)))

(defun num-2-3nums-groups (num) ;(num-2-3nums-groups 1023405789) -> (1 23 405 789)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (num-2-3nums-groups
			  (floor num 1000))
			(list (mod num 1000))))))

(defun 3nums-groups-2-text (nums)
       (append (3first-nums-2-text (first nums))
	       (list (add-unit (length nums)))
	       (rest-3nums-groups-2-text (rest nums))))

(defun rest-3nums-groups-2-text (nums)
       (cond ((zerop (length nums)) nil)
	     (t (append (3nums-2-text (first nums))
			(cons (add-unit (length nums))
			      (rest-3nums-groups-2-text (rest nums)))))))

(setf text-4-units
      '((5 nghìn-tỷ)
	(4 tỷ)
	(3 triệu)
	(2 ngàn)
	(1 đơn-vị)))

(defun add-unit (a)
       (second (assoc a text-4-units)))

(setf text-4-nums-table
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

(defun 3first-nums-2-text (num)
       (cond ((< num 10) (list (second (assoc num
					      text-4-nums-table))))
	     ((< num 100) (append (read-second-num num)
				  (read-third-num num)))
	     (t (3nums-2-text num))))

(defun 3nums-2-text (num)
       (cond ((zerop num) nil)
	      (t (append (read-first-num num)
			 (read-second-num num)
			 (read-third-num num)))))

(defun read-first-num (num)
       (list (second (assoc (floor num 100)
			    text-4-nums-table)) 'trăm))

(defun read-second-num (num)
       (third (assoc (mod (floor num 10) 10)
		     text-4-nums-table)))

(defun read-third-num (num)
       (cond ((zerop (mod num 10)) nil)
	     ((or (equalp (mod num 100) 11)
		  (equalp num 1)) '(một))
	     (t (list (fourth (assoc (mod num 10)
				     text-4-nums-table))))))

