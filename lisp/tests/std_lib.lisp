;; not

(print (not (< 3 5)))
(print (not (< 5 3)))

;begin
NIL
T
;end

;; mapcar

(print (mapcar (lambda (x) (* x x)) '(1 2 3 4 5)))

;begin
(1 4 9 16 25)
;end

;; when

(when (< 3 5) (print 15) (print 77))
(when (< 5 3) (print 16) (print 78))

;begin
15
77
;end

;; unless

(unless (< 3 5) (print 15) (print 77))
(unless (< 5 3) (print 16) (print 78))

;begin
16
78
;end

;; reverse

(print (reverse '(1 2 3 4)))

;begin
(4 3 2 1)
;end

;; incf and 1+

(let ((x 15)) (print (incf x)) (print x))
(let ((x 25)) (print (1+ x)) (print x))

;begin
16
16
26
25
;end

;; nth

(print (nth 3 '(0 1 2 3 4 5)))
(print (nth 8 '(0 1 2 3 4 5)))

;begin
3
NIL
;end

;; list-length

(print (list-length (list)))
(print (list-length '(1 2 3 4)))

;begin
0
4
;end

(do ((i 0 (+ 3 i))) ((> i 10) i) (print i))

;begin
12
;end