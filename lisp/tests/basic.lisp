(print 77) (print 34)

;begin
77
34
;end

(let ((x 34)) (print (+ 10 x)))

;begin
44
;end

(defun f () (+ 10 55))
(defun g (x y) (+ x (+ y y)))
(defun h (x y) (+ x (+ y y)))

(print (f))
(print (g 5 3))
(let ((a 3) (b 7)) (print (h a b)))

;begin
65
11
17
;end

;; LET

(print (let ((x 12) (y 3)) (+ x y) (+ (+ x x) y)))
(print (let ((a (* 3 (+ 2 5)))) a))
(print (let ((x 10)) (let ((y 4)) (+ x y))))

;begin
27
21
14
;end

;; LIST

(print (list 3 4 5))
(print (let ((x 2)) (list 1 x 3)))
(print (list 10 20 (list 30 40)))
(print (list))

;begin
(3 4 5)
(1 2 3)
(10 20 (30 40))
NIL
;end

;; SETQ

(print (let ((a 10)) (setq a 12)))
(print (let ((a 10)) (setq a 12) (+ 30 a)))
(print (let ((x 2) (y 6)) (progn (setq x 4 y 8) (+ x y))))

;begin
12
42
12
;end

;; IF

(print (if (> 10 3) 5 100))
(print (if (> 2 44) 5 100))

;begin
5
100
;end

;; QUOTE

(print (quote ret))
(print (quote (+ x 5)))
(print (let ((x 12)) (quote x)))
(print (quote (x)))
(print (quote (quote (x))))

;begin
ret
(+ x 5)
x
(x)
(quote (x))
;end