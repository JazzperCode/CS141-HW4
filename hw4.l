(defun my-length(L)
	(cond 
		((eq L nil)0)
		(t (+ 1 (my-length(cdr L))))
	)
)

(print "My Length")
(print (my-length nil))
(print (my-length ' (b (a b c))))
(print (my-length ' (a(((b))) c)))
(print (my-length ' (a b c)))

(defun my-memq (a L)
	(cond
		((eq L nil)nil)
		((eq (car L) a) L)
		(t (my-memq a (cdr L)))
	)
)

(print "My Memq")
(print (my-memq 'a nil))
(print (my-memq 'b '(a b c)))
(print (my-memq 'd '(a b c d e f g)))
(print (my-memq 'd '(a b c d)))
(print (my-memq 'd '(a b c)))

(defun my-append (L1 L2)
	(cond
		((eq L2 nil) L1)
		((eq L1 nil)(cons (car L2) (my-append L1 (cdr L2))))
		(t (cons (car L1) (my-append (cdr L1) L2)))
	)
)

(print"My Append")
(print(my-append '(a b c) '(d e f)))
(print(my-append '((a) (b) (c)) '((d) (e) (f))))
(print(my-append nil '(d e f)))
(print(my-append '(a b c) nil))

(defun my-attach (O L)
	(cond
		((eq L nil) (cons O nil))
		(t (cons (car L) (my-attach O (cdr L))))
	)
)

(print"My Attach")
(print(my-attach 'a nil))
(print(my-attach 'd '(a b c)))
(print(my-attach '(a) '(b c)))

(defun my-assoc (A L)
	(cond
		((eq L nil)nil)
		((eq A (car(car L)))(car L))
		(t (my-assoc A (cdr L)))
	)
)

(print "My Assoc")
(print(my-assoc 'a nil))
(print(my-assoc 'a '((a.b)(c e f)(b))))
(print(my-assoc 'c '((a . b)(c e f)(b))))
(print(my-assoc 'b '((a . b)(c e f)(b))))
(print(my-assoc 'f '((a . b)(c e f)(b))))

(defun freq (A L)
	(cond
		((eq L nil)0)
		((listp (car L)) (+ (freq A (cdr L))(freq A (car L))))
		((eq (car L) A)(+ 1 (freq A (cdr L))))
		(t(freq A (cdr L)))
	)
)

(print "Freq")
(print(freq  'c '((a c) c e)))
(print(freq  'f '(((s) o ) d)))
(print(freq  'f '(((f) f) f f)))

(defun mapping (L val)
	(cond
		((eq L nil)nil)
		((> val (car (car L))) (cons (car(cdr(car L))) (mapping (cdr L) val)))
		(t (mapping (cdr L) val)) 
	)
)

(print(mapping '((35 kim) (67 clinton) (45 emma))  40))
(print(mapping '((24 a) (15 b) (56 c) (19 d)) 26))
(print(mapping '((90 a) (80 b) (70 c))  40))

(print "My Last")
(defun my-last (A L)
	(cond
		((eq (my-memq A (cdr L)) nil) (my-memq A L))
		(t(my-last A (cdr L)))
	)
)

(print(my-last 'a '(a b c a b c a b c d e f g)))
(print(my-last 'b '(a b c a b c a b c d e f g)))
(print(my-last 'c '(a b c a b c a b c d e f g)))
(print(my-last 'g '(a b c a b c a b c d e f g)))
(print(my-last 'h '(a b c a b c a b c d e f g)))

(defun my-reverse (L)
	(cond
		((eq L nil)nil)
		(t(my-attach (car L) (my-reverse(cdr L))))
	)
)
(print "My reverse")
(print(my-reverse nil))
(print(my-reverse '(a)))
(print(my-reverse '(1 2 3 4 5)))
(print(my-reverse '((1 2 3) 4 ((5 6)))))

(defun pattern_help (pat str)
	(cond
		((eq (car pat) (car str))(pattern_help (cdr pat) (cdr str)))
		((eq pat nil)t)
		((eq str nil)nil) 
	)
)

(defun is-pattern? (pat str)
	(cond 
	    ((eq pat nil)nil)
	    ((eq str nil)nil)
		((pattern_help pat str)str)
		(t(is-pattern? pat (cdr str)))
	)
)

(print "Is pattern?")
(print(is-pattern? '(a b s) '(c d b a s)))
(print(is-pattern? '(c a c) '(b a j a c a c t u s)))
(print(is-pattern? nil '(a n y l i s t)))
(print(is-pattern? '(l i s p) nil))

(defun first-atom (L)
	(cond 
		((eq L nil)nil)
		((listp (car L))(first-atom(car L)))
		(t(car L))
	)
)

(print (first-atom nil))
(print(first-atom '((2 (1) 4) 6)))
(print(first-atom '((((s)) o ))))
(print(first-atom '(1 (((2)) 3 4))))

(defun find_helper (L)
    (cond
        ((eq (listp(car L)) nil)(cdr L))
        ((eq (find_helper(car L)) nil)(cdr L))
        (t (cons (find_helper (car L)) (cdr L)))
    )
)

(defun find-all (A L)
	(cond 
	    ((eq L nil)nil)
		((eq (first-atom L) A)(cons (first-atom(find_helper L))(find-all A (find_helper L))))
		(t(find-all A (find_helper L)))
	)
)

(print(find-all 'a nil))
(print(find-all 'a '(b a c a e)))
(print(find-all 'a '(b d c e)))
(print(find-all 'a '(b (a a) c)))
(print(find-all 'a '((b a) ((c a b)))))