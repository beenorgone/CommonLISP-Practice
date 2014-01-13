"(defun num->text [num] ...)"
"(num->text 1,034,567,890) -> 'một tỷ không trăm ...'"

(defun num->word (num)
       (append (groups-of-nine->word (num->groups-of-nine num))
	       measure-unit))

(setf measure-unit '(đơn vị)) ;replace đơn-vị with m2, m3, vnd, usd, ...

;Use function NUM->GROUPS-OF-NINE to separate NUM into numbers (each of them has at most 9 digits) (from the end of NUM). (num->groups-of-nine 1000234003) -> (1 234003)

(defun num->groups-of-nine (num)
       (cond ((zerop (floor num 1000000000)) (list num))
	     (t (append (num->groups-of-nine (floor num 1000000000))
			(list (mod num 1000000000))))))

;Test cases
(num->groups-of-nine 1000234003) ;(1 234003)

;Write function GROUPS-OF-NINE->WORD to convert the groups of nine into words.
;Solution:
;- add unit (tỷ) for each group except the last.
;- Write functions FIRST-GROUP-OF-NINE->WORD & REST-GROUPS-OF-NINE->WORD to deal with some special cases.
;- Separate per group of nine into trios.
;- Write function GROUP-OF-NINE->WORD to convert a group of nine into words. This function use two functions TRIOS->WORD & NUM->TRIOS.

(defun groups-of-nine->word (nums)
       (if (equalp 1 (length nums))
	   (first-group-of-nine->word (first nums))
	   (append (first-group-of-nine->word (first nums))
		   '(tỷ) ;add unit for first group.
		   (rest-groups-of-nine->word (rest nums)))))

(defun first-group-of-nine->word (num)
       (cond ((< num 1000) (first-trio->word num))
	     ((< num 1000000) (trios->word (num->trios num)))
	     (t (group-of-nine->word num))))

(defun rest-groups-of-nine->word (nums)
       (cond ((equalp (length nums) 1) (if (zerop (first nums))
					   nil ;ignore if the last group is 0.
					   (group-of-nine->word (first nums))))
	     ((zerop (first nums)) (append '(tỷ) ;add group's unit but ignore the group if it's zero.
					   (rest-groups-of-nine->word (rest nums))))
	     (t (append (group-of-nine->word (first nums))
			'(tỷ) ;add unit for each group (except the last).
			(rest-groups-of-nine->word (rest nums))))))

;Separate per group of nine into trios.

(defun num->trios (num)
       (cond ((zerop (floor num 1000)) (list num))
	     (t (append (num->trios (floor num 1000))
			(list (mod num 1000))))))

;Write function GROUP-OF-NINE->WORD to convert a group of nine into words. This function use two functions TRIOS->WORD & NUM->TRIOS.

(defun group-of-nine->word (num)
       (cond ((< num 1000) (append '(không triệu không nghìn)
				   (trio->word num)))
	     ((< num 1000000) (append '(không triệu)
				      (trios->word (num->trios num))))
	     (t (trios->word (num->trios num)))))

;Convert trios into words with TRIOS->WORD function.
;Solution:
;- add unit (triệu, ngàn) for each trio except special cases.
;- Write functions FIRST-TRIO->WORD & REST-TRIOS->WORD to deal with special cases.

(defun trios->word (nums)
       (if (equalp 1 (length nums))
	   (first-trio->word (first nums))
	   (append (first-trio->word (first nums))
		   (add-unit (length nums))
		   (rest-trios->word (rest nums)))))

(setf unit-table
      '((3 (triệu))
	(2 (nghìn))
	(1)))

(defun add-unit (a)
       (second (assoc a unit-table)))

"Test cases"
(add-unit 1)	;NIL
(add-unit 2)	;
(add-unit 6)	;nil

(defun first-trio->word (num)
       (cond ((< num 10)
	      (list (second (assoc num digit->word-table))))
	     ((< num 100) (append (second-digit->word num)
				  (third-digit->word num)))
	     (t (trio->word num))))

(defun rest-trios->word (nums)
       (cond ((or (and (equalp (length nums) 2)
		       (zerop (apply #'+ nums))) ;ignore (replace with nil) if the two last trios are zero both.
		  (and (equalp (length nums) 1)
		       (zerop (first nums))) ;ignore (replace with nil) if the last trio is 0.
		  (null nums))
	      nil)
	     (t (append (trio->word (first nums))
			(add-unit (length nums))
			(rest-trios->word (rest nums))))))

;Use TRIO-WORD to convert a trio into words.

(defun trio->word (num)
       (cond ((zerop num) '(không))
	     (t (append (first-digit->word num)
			(second-digit->word num)
			(third-digit->word num)))))

;Test Cases
(trio->word 3)		;(KHÔNG TRĂM LINH BA)
(trio->word 11)		;(KHÔNG TRĂM MƯỜI MỘT)
(trio->word 910)		;(CHÍN TRĂM MƯỜI)
(trio->word 914)		;(CHÍN TRĂM MƯỜI BỐN)
(trio->word 924)		;(CHÍN TRĂM HAI MƯƠI TƯ)

(setf digit->word-table
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

(defun first-digit->word (num)
       (list (second (assoc (floor num 100)
			    digit->word-table)) 'trăm))

;Test Cases
(first-digit->word 3)	;(KHÔNG TRĂM)
(first-digit->word 23)	;(KHÔNG TRĂM)
(first-digit->word 910)	;(CHÍN TRĂM)

(defun second-digit->word (num)
       (if (zerop (mod num 100)) ;If the third & the second of the group are zero, ignore both.
	   nil
	   (third (assoc (mod (floor num 10) 10)
			 digit->word-table))))

;Test Cases
(second-digit->word 3)	;(LINH)
(second-digit->word 23)	;(HAI MƯƠI)
(second-digit->word 910)	;(MƯỜI)

(defun third-digit->word (num)
       (cond ((zerop (mod num 10)) nil)
	     ((or (equalp (mod num 100) 11)
		  (equalp (mod num 100) 1)) '(một))
	     ((or (equalp (mod num 100) 14)
		  (equalp (mod num 100) 4)) '(bốn))
	     ((equalp (mod num 100) 5) '(năm))
	     (t (list (fourth (assoc (mod num 10)
				     digit->word-table))))))

;Test Cases
(third-digit->word 3)	;(BA)
(third-digit->word 11)	;(MỘT)
(third-digit->word 910)	;NIL
(third-digit->word 914)	;(BỐN)
(third-digit->word 924)	;(TƯ)