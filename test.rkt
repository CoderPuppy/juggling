#lang racket

(require pict)
(require ppict/2)
(require racket/draw)
(require racket/block)

(define (draw siteswap)
	(define picts (for/vector ([beat siteswap])
		(text (if (= (length beat) 1)
			(format "~a" (first beat))
			(string-append
				"["
				(apply string-append
					(for/list ([throw beat])
						(format "~a" throw)))
				"]")))))
	(define base-visual (apply hc-append (vector->list picts)))
	(define visual (for/fold
		([visual base-visual])
		(
			[beat siteswap]
			[i (in-naturals)]
			[beat-pict picts]
			#:when #t
			[throw beat]
			#:when (< (+ i throw) (vector-length picts))
		)
		(let*
			(
				[dest-beat-pict (vector-ref picts (+ i throw))]
				[mul (expt -1 i)]
				[finder (if (= (modulo i 2) 0) ct-find cb-find)]
			)
			(pin-arrow-line 10 visual
				beat-pict finder
				dest-beat-pict finder
				#:start-angle (* mul 1/2 pi) #:end-angle (* mul -1/2 pi)
				#:start-pull 6/5 #:end-pull 5/4))))

	(inset visual 0 70 0 70))
(define siteswap (for/list
	(
		[i (in-range 20)]
		#:when #t
		[beat '((3) (4 3) (2))]
	)
	beat))

(define draw-siteswap (let
	(
		[beat-width 40]
		[throw-height 40]
		[first-beat-x 5]
		[draw-text (lambda (dc text x y)
			(define-values (w h b a) (send dc get-text-extent text))
			(send dc draw-text
				text
				(+ x (* -1/2 w))
				y))]
	)
	(lambda (siteswap)
		(define throws-height (* throw-height (apply max (flatten siteswap))))
		(dc
			(λ (dc dx dy)
				(define old-brush (send dc get-brush))
				(define old-pen (send dc get-pen))
				(send dc set-pen (new pen% [width 1] [color "slategray"]))
				(for
					(
						[beat siteswap]
						[place (in-naturals)]
						#:when #t
						[throw beat]
					)
					(send dc draw-spline
						(+ dx first-beat-x (* beat-width place))
						(+ dy throws-height)

						(+ dx first-beat-x (* beat-width place) (* 1/2 beat-width throw))
						(+ dy throws-height (* -1 throw-height throw))

						(+ dx first-beat-x (* beat-width place) (* beat-width throw))
						(+ dy throws-height))
					(draw-text dc (if (= (modulo place 2) 0) "L" "R")
						(+ dx first-beat-x (* beat-width place))
						(+ dy throws-height))
					(draw-text dc (format "~a" (+ place 1))
						(+ dx first-beat-x (* beat-width place))
						(+ dy throws-height 20))
					(draw-text dc (format "~a" throw)
						(+ dx first-beat-x (* beat-width place) (* 1/2 beat-width throw))
						(+ dy throws-height (* -2/3 throw-height throw))))
				(send dc set-brush old-brush)
				(send dc set-pen old-pen))
			(* beat-width (length siteswap))
			(+ throws-height 40)))))

(draw-siteswap siteswap)
