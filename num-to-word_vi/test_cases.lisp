"(defun num->text [num] ...)"
"(num->text 1,034,567,890) -> 'một tỷ không trăm ...'"

";Test cases
(num->groups-of-nine 1000234003) ;(1 234003)"

"Test cases
(add-unit 1)	;NIL
(add-unit 2)	;
(add-unit 6)	;nil"

";Test Cases
(trio->word 3)		;(KHÔNG TRĂM LINH BA)
(trio->word 11)		;(KHÔNG TRĂM MƯỜI MỘT)
(trio->word 910)		;(CHÍN TRĂM MƯỜI)
(trio->word 914)		;(CHÍN TRĂM MƯỜI BỐN)
(trio->word 924)		;(CHÍN TRĂM HAI MƯƠI TƯ)"



";Test Cases
(first-digit->word 3)	;(KHÔNG TRĂM)
(first-digit->word 23)	;(KHÔNG TRĂM)
(first-digit->word 910)	;(CHÍN TRĂM)"



";Test Cases
(second-digit->word 3)	;(LINH)
(second-digit->word 23)	;(HAI MƯƠI)
(second-digit->word 910)	;(MƯỜI)"



";Test Cases
(third-digit->word 3)	;(BA)
(third-digit->word 11)	;(MỘT)
(third-digit->word 910)	;NIL
(third-digit->word 914)	;(BỐN)
(third-digit->word 924)	;(TƯ)"