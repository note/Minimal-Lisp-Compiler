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

;; LAMBDA

(print ((lambda (x) (* x x)) 3))
(print ((lambda (x y) (+ (* 2 x) y)) 4 (+ 4 7)))
(print (let ((x 32)) ((lambda (y) (* x y)) 3)))
(print (let ((x ((lambda (x) (* x x)) 12))) x))
(print (let ((ff (lambda (x) (* 2 x)))) (funcall ff 5)))

;begin
9
19
96
144
10
;end

;; FUNCALL

(print (let ((fn (lambda (x) (* x x)))) (funcall fn 4)))

;begin
16
;end

;; DEFMACRO

(defun __reverse (l res) (if (car l) (__reverse (cdr l) (cons (car l) res)) res))
(defun reverse (l) (__reverse l (list)))

(defmacro backwards (x) (reverse x))

(print (reverse (list 4 5 6)))
(backwards (56 print))

;begin
(6 5 4)
56
;end

;; ' instead of quote

(print 'ret)
(print '(+ x 5))
(print (let ((x 12)) 'x))
(print '(x))
(print '(quote (x)))

;begin
ret
(+ x 5)
x
(x)
(quote (x))
;end

;; BACKQUOTE

(defun g () (list 5 6 7))
(print `(+ x 5))
(print (let ((x 12)) `x))
(print `x)
(print `(+ 4 ,(* 2 5)))
(print `(+ (* 3 3) ,(* 2 5)))
(print `(* (+ 4 ,(+ 3 7)) 11))
(print `(3 4 ,(list 7 8)))
(print `(3 4 ,@(list 7 8)))
(print `(,(g) 100))
(print `(,@(g) 100))

;begin
(+ x 5)
x
x
(+ 4 10)
(+ (* 3 3) 10)
(* (+ 4 10) 11)
(3 4 (7 8))
(3 4 7 8)
((5 6 7) 100)
(5 6 7 100)
;end