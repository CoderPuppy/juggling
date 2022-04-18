#lang racket

(require racket/gui)
(require racket/block)

(struct point (time hand) #:transparent)
(struct line (start end color height) #:transparent)

(define (line-wrap? l)
	(< (point-time (line-end l)) (point-time (line-start l))))
(define (line-duration l)
	(if (line-wrap? l)
		(+
			(point-time (line-end l))
			(- 1 (point-time (line-start l))))
		(-
			(point-time (line-end l))
			(point-time (line-start l)))))
(define (line-active? l t)
	(if (line-wrap? l)
		(or
			(<= t (point-time (line-end l)))
			(>= t (point-time (line-start l))))
		(and
			(<= t (point-time (line-end l)))
			(>= t (point-time (line-start l))))))
(define (line-cur l t)
	(-
		(if (and (line-wrap? l) (<= t (point-time (line-end l))))
			(+ t 1)
			t)
		(point-time (line-start l))))

(define t 0)
(define lines (mutable-set))
(define (add-line start end color height)
	; (when (< (point-time end) (point-time start))
	; 	(define t start)
	; 	(set! start end)
	; 	(set! end t))
	(define l (line start end color height))
	(printf "~a: wrap = ~a, dur = ~a~n" l (line-wrap? l) (line-duration l))
	(set-add! lines l))

(define ball-color "red")
(define neg-color "blue")
(define ghost-color "purple")
(define num-hands 2)
(define siteswap (list 5 2 2 (list 4 -4) 4 5 3))
(define num-balls (/ (foldl + 0 (flatten siteswap)) (length siteswap)))
(define long-siteswap (for/list
	(
		; [i (in-range 1)]
		[i (in-range (/ (lcm (length siteswap) (apply max (flatten siteswap)) num-hands) (length siteswap)))]
		#:when #t
		[throw siteswap]
	)
	throw))
(displayln long-siteswap)
(define beat-time (/ 1 (length long-siteswap)))
(define dwell-time (* beat-time 1/5))
(define throw-time (- beat-time dwell-time))
(define points (for/vector
	(
		[throw long-siteswap]
		[i (in-naturals)]
		#:when #t
		[ct (in-range 2)]
	)
	(point (+ (* i beat-time) (* ct dwell-time)) (modulo i num-hands))))
(displayln points)
(for
	(
		[item long-siteswap]
		[i (in-naturals)]
		#:unless (eq? item 0)
	)
	; (printf "carry ~a ~a~n" (modulo (- (* i 2) 1) (vector-length points)) (* i 2))
	(add-line
		(vector-ref points (* i 2))
		(vector-ref points (+ (* i 2) 1))
		ball-color
		0
	)
	(for
		(
			[throw (if (list? item) item (list item))]
			#:unless (eq? throw 0)
		)
		(if (> throw 0)
			(begin
				(add-line
					(vector-ref points (+ (* i 2) 1))
					(vector-ref points (modulo (* (+ i throw) 2) (vector-length points)))
					; (make-object color%
					; 	(modulo (* i 30) 256)
					; 	(modulo (* i 50) 256)
					; 	(modulo (* i 12) 256))
					ball-color
					(if (> throw 2)
						(* throw 50)
						0)))
			(block
				; (printf "neg throw ~a: ~a ~a~n" throw (modulo (- (* (+ i throw) 2) 1) (vector-length points)) (* i 2))
				(add-line
					(vector-ref points (modulo (* (+ i throw) 2) (vector-length points)))
					(vector-ref points (+ (* i 2) 1))
					neg-color
					(if (< throw -2)
						(* throw -50)
						0))
				(define ghost (modulo throw (lcm (length siteswap) 2)))
				; (printf "ghost throw ~a: ~a ~a~n" ghost (* i 2) (modulo (- (* (+ i ghost) 2) 1) (vector-length points)))
				(add-line
					(vector-ref points (+ (* i 2) 1))
					(vector-ref points (modulo (* (+ i ghost) 2) (vector-length points)))
					ghost-color
					(if (> ghost 2)
						(* ghost -50)
						0))
				))))
(define rate (* beat-time 0.01))

(define hands-x (vector 200 300 #;400))
(define hands-y 400)

(define frame (new frame% [label "Juggling"]))
(define no-pen (new pen% [style 'transparent]))
(define canvas (new canvas%
	[parent frame]
	[paint-callback (lambda (canvas dc)
		(send dc clear)
		(for
			(
				[l lines]
				#:when (line-active? l t)
			)

			(send dc set-pen no-pen)
			(send dc set-brush (line-color l) 'solid)
			(define cur (line-cur l t))
			(define dur (line-duration l))
			(define x1 (vector-ref hands-x (point-hand (line-start l))))
			(define x2 (vector-ref hands-x (point-hand (line-end l))))
			(send dc draw-ellipse
				(+ x1 (* (/ cur dur) (- x2 x1)))
				(+ hands-y (* (/ 4 dur dur) (line-height l) cur (- cur dur)))
				10 10)
			)
		(for
			([hand-x hands-x])

			(send dc set-pen "black" 2 'solid)
			(send dc draw-line hand-x hands-y hand-x (+ hands-y 15))
			))]))
(new timer%
	[interval 10]
	[notify-callback (lambda ()
		(set! t
			(if (>= t 1)
				0
				(+ t rate)))
		(send canvas on-paint)
		)])
(send frame show #t)
