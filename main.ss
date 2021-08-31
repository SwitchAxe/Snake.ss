(import
	(rnrs)
	(only (chezscheme) format)
	(ncurses))

(define (sleep-twice nsec)
    (sleep (make-time 'time-duration nsec 0))
    (sleep (make-time 'time-duration nsec 0)))

(define (yx->x coords)
	(+ (* (car coords) COLS) (cadr coords)))
(define (x->yx index)
	(list (floor (/ index COLS)) (modulo index COLS)))

(define (list-pop l)
    (list-head l (- (length l) 1)))

(define (snake-has-eaten-apple? snake-l apple-l)
	(if (and
		(= (car (x->yx (car snake-l))) (car (x->yx (car apple-l))))
		(= (cadr (x->yx (car snake-l))) (cadr (x->yx (car apple-l))))) #t #f))

(define (safe-getch)
    (call/cc (lambda (k)
               (with-exception-handler
                 (lambda _ (k #f))
                 getch))))
                 
(define (nb-getch)
	(let ((ch (safe-getch)))
		(if (equal? ch #f)
			1
			ch)))




(define (gen-apple)
	(list (random (* LINES COLS))))

(define (grow-snake snake-l pressed-key)
	(cond
		((= (length snake-l) 1)
			(cond
				((or (= pressed-key (char->integer #\w)) (= pressed-key KEY_UP))
					(list (car snake-l) (- (car snake-l) COLS)))
				((or (= pressed-key (char->integer #\a)) (= pressed-key KEY_LEFT))
					(list (car snake-l) (- (car snake-l) 1)))
				((or (= pressed-key (char->integer #\s)) (= pressed-key KEY_DOWN))
					(list (car snake-l) (+ (cadr snake-l) COLS)))
				((or (= pressed-key (char->integer #\d)) (= pressed-key KEY_RIGHT))
					(list (car snake-l) (+ (car snake-l) 1)))))
		(else
			(if (=
					;;if the two last elements of the snake are aligned horizontally
					(cadr (x->yx (list-ref snake-l (- (length snake-l) 1))))
					(cadr (x->yx (list-ref snake-l (- (length snake-l) 2)))))
						(append snake-l (list (+ (list-ref snake-l (- (length snake-l) 1)) 1)))
						(append snake-l (list (+ (list-ref snake-l (- (length snake-l) 1)) COLS)))))))

(define (get-direction pressed-key prev-direction)
	(cond
		((or (equal? prev-direction "up") (equal? prev-direction "down"))
			(cond
				((or (= pressed-key (char->integer #\a)) (= pressed-key KEY_LEFT)) "left")
				((or (= pressed-key (char->integer #\d)) (= pressed-key KEY_RIGHT)) "right")
				(else (if (equal? prev-direction "up") "up" "down"))))
		((or (equal? prev-direction "left") (equal? prev-direction "right"))
			(cond
				((or (= pressed-key (char->integer #\w)) (= pressed-key KEY_UP)) "up")
				((or (= pressed-key (char->integer #\s)) (= pressed-key KEY_DOWN)) "down")
				(else (if (equal? prev-direction "left") "left" "right"))))))

(define (update-snake snake-l pressed-key prev-direction)
	(cond
		((equal? (get-direction pressed-key prev-direction) "up")
			(list-pop (cons (- (car snake-l) COLS) snake-l)))
		((equal? (get-direction pressed-key prev-direction) "left")
			(list-pop (cons (- (car snake-l) 1) snake-l)))
		((equal? (get-direction pressed-key prev-direction) "down")
			(list-pop (cons (+ (car snake-l) COLS) snake-l)))
		((equal? (get-direction pressed-key prev-direction) "right")
			(list-pop (cons (+ (car snake-l) 1) snake-l)))
		(else snake-l)))

(define (range-aux x ci)
	(cond
    	((= x ci) '()) (else (cons ci (range-aux x (+ ci 1))))))

(define (range x) (range-aux x 0))

(define (clear-grid)
	(for-each
		(lambda (x)
			(mvaddch (car (x->yx x)) (cadr (x->yx x)) (char->integer #\space)))
		(range (+ (* LINES COLS) 1))))

(define (print-grid snake-l apple-l)
	(let ((apple-x (cadr (x->yx (car apple-l))))
		  (apple-y (car (x->yx (car apple-l))))
		  (snake-x (cadr (x->yx (car snake-l))))
		  (snake-y (car (x->yx (car snake-l)))))
		(clear-grid)
		(mvaddstr 1 1 (format "apple y: ~a, apple x: ~a" apple-y apple-x))
		(mvaddstr 2 1 (format "snake y: ~a, snake x: ~a" snake-y snake-x))
		(for-each
			(lambda (elem)
				(mvaddch (car (x->yx elem)) (cadr (x->yx elem))
				(char->integer #\o)))
			snake-l)
		(mvaddch apple-y apple-x #\@)))

(define (lp snake-l apple-l direction)
	(let* ((ch (nb-getch))
		   (new-direction (get-direction ch direction)))
		(print-grid snake-l apple-l)
		(sleep-twice 10000000)
		(cond
			((= ch (char->integer #\newline)) ;;exit condition
				(void))
			((snake-has-eaten-apple? snake-l apple-l)
				(lp
				    (grow-snake (update-snake snake-l ch new-direction) ch)
				    (gen-apple) (get-direction ch new-direction)))
			(else (lp
			        (update-snake snake-l ch (get-direction ch new-direction))
			        apple-l new-direction)))))

;;sets the random seed
(random-seed (time-second (current-time)))
;;starts ncurses mode
(define win (initscr))
(assert (equal? win stdscr))
(keypad stdscr #t)
(noecho)
(cbreak)
(nodelay stdscr #t)

(start-color)
(curs-set 0)
(use-default-colors)
(lp (x->yx (yx->x '(10 10))) (gen-apple) "down")
(endwin)
(exit)
