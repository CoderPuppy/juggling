#lang slideshow

(require threading)
(require ppict/2)
(require racket/draw)
(require slideshow/step)
(require racket/match)
(require pict/shadow)
(require latex-pict)
(require srfi/26)

(define (unify-size ps)
	(define max-width (apply max (for/list ([p ps]) (pict-width (second p)))))
	(define max-height (apply max (for/list ([p ps]) (pict-height (second p)))))
	(define back (blank max-width max-height))
	(for/list ([p ps])
		((first p) back (second p))))

; block = (combine . stmts)
; stmt = pict
;      | 'next
;      | ('do . block)
;      | ('tail combine)
;      | ('embed . picts)
;      | ('alt . stmts)
;      | ('map fn)

; (list (union 'next (list pict)))

(define (process-core combine items [pre '()])
	(match items
		[(cons (list p) items)
			(process-core combine items (append pre (list p)))]
		[(cons (cons p ps) items)
			(let-values
				([(post-items ghost-items) (splitf-at items (λ~> (eq? 'next) not))])
				(cons
					(apply combine (append
						pre (list p)
						(map first post-items)
						(for/list
							([item ghost-items] #:unless (eq? item 'next))
							(ghost (first item)))))
					(process-core combine (cons ps items) pre)))]
		[(cons 'next items)
			(cons
				(apply combine (append
					pre
					(for/list
						([item items] #:unless (eq? item 'next))
						(ghost (first item)))))
				(process-core combine items pre))]
		[(list) (list (apply combine pre))]))
(define (process combine stmts)
	(process-core combine (preprocess combine stmts)))
(define (preprocess-alts stmts [alts '()])
	(match stmts
		[(cons (cons 'alt alt-stmts) stmts)
			(preprocess-alts stmts (cons alt-stmts alts))]
		[stmts
			(cons stmts alts)]))
(define (preprocess combine stmts)
	(match stmts
		[(cons (list* 'do in-combine in-stmts) stmts)
			(cons
				(process in-combine in-stmts)
				(preprocess combine stmts))]
		[(cons (list 'tail combine) stmts)
			(list (process combine stmts))]
		[(and stmts (cons (cons 'alt _) _))
			(list
				(unify-size (for/list
					(
						[alt (reverse (preprocess-alts stmts))]
						#:when #t
						[pict (process combine (rest alt))]
					)
					(list (first alt) pict))))]
		[(cons 'next stmts)
			(cons 'next (preprocess combine stmts))]
		[(cons (? pict? pict) stmts)
			(cons (list pict) (preprocess combine stmts))]
		[(cons (list 'map fn) stmts)
			(list (map fn (process combine stmts)))]
		[(list) (list)]))
(define
	(myslide
		#:ignore? [ignore? #f]
		#:title [title #f]
		#:name [name title]
		combine
		. body)
	(unless ignore?
		(for ([pict (process combine body)])
			(slide
				#:title title
				#:name name
				#:layout 'tall
				pict))))
(set-margin! 0)
(define shadow-color (make-object color% 20 20 20))
(define title-height (+ 80 (* 2 30)))
(define titled-body-height (- client-h title-height))
(define wat-save #f)
(current-slide-assembler
	(let ([orig (current-slide-assembler)])
		(λ (title sep body) (let*
			(
				[pict body]
				[bg (blank client-w client-h)]
				[pict (ct-superimpose bg pict)]
				[pict (refocus pict bg)]
				[pict (if title
					(let*
						(
							[background (shadow
								(filled-rounded-rectangle
									(- client-w 40) 80 3
									#:draw-border? #f)
								15
								#:color "indigo"
								#:shadow-color shadow-color)]
							[t (inset
								(shadow
									(text title (list 'bold) 30)
									30
									#:color "ivory"
									#:shadow-color shadow-color)
								35 0)]
							[title-pict (inset (lc-superimpose background t) 20 30)]
						)
						(vl-append title-pict pict))
					pict)]
				[bg (filled-rectangle
						client-w
						client-h
						#:draw-border? #f
						#:color "bisque")]
				[pict (ct-superimpose bg pict)]
			)
			(set! wat-save body)
			pict))))

(define (spline cx cy dx dy)
	(dc
		(λ (dc x y)
			(send dc draw-spline
				x y
				(+ x cx) (+ y cy)
				(+ x dx) (+ y dy)))
		0 0))

(define (repeat n l)
	(for/list
		(
			[i (in-range n)]
			#:when #t
			[v l]
		)
		v))

(define beat-width (make-parameter 40))
(define throw-height (make-parameter 40))
(define siteswap-text-style (make-parameter 'default))
(define siteswap-text-size (make-parameter 20))

(define (throws . throws)
	(apply lb-superimpose
		(for/list ([throw throws])
			(inset
				(spline
					(* 1/2 (beat-width) throw)
					(* -1 (throw-height) throw)

					(* (beat-width) throw)
					0)
				0 (* (throw-height) throw) 0 0))))

(define (siteswap-arcs ss)
	(apply hb-append (beat-width)
		(for/list ([throw ss])
			(throws throw))))

(define (zero-width p [center 1/2])
	(inset p
		(* -1 center (pict-width p)) 0
		(* -1 (- 1 center) (pict-width p)) 0))

(define (siteswap-numbers ss)
	(apply hb-append (beat-width)
		(for/list ([throw ss] [i (in-naturals)])
			(zero-width (text (format "~a" throw) (siteswap-text-style) (siteswap-text-size))))))

(define ss '(4 2 3 4 2 3 3 4 2 4 2 3 3 4 2 3 4 2 3 3 3 4 2 3 3 3 3 3 3 3 3 4 2 3 3 3 4 2 3 3 3 3 3 3 3 3 3 4 2 3 3 3 3 3 4 2
	))

(current-preamble "\\usepackage{amsmath} \\usepackage{amsfonts}")

; intro to siteswap
; 	what is siteswap?
; 		notation
; 		flow: cascade, reverse cascade, windmill, flo's
; 		height
; 	infinite
; 		hands
; 		trace throw
; 	finite
; 		trace
; 		modular arithmetic
; 		permutation
; 		validity
; equivalence
; 	repetition
; 	scaling
; modification
; 	swap
; 		extended swap
; 	add period (modulo)
; equivalence pt2
; 	removing 0s
; 	merging throws
; 		0s and 2s
; future work

(myslide #:name "Title"
	; #:ignore? #t
	vc-append
	(blank 0 100)
	(let*
		(
			[t (inset
				(shadow
					(text "Math in Juggling" (list* 'bold 'default) 60)
					30
					#:color "ivory"
					#:shadow-color shadow-color)
				50 50)]
			[background (shadow
				(filled-rounded-rectangle
					(pict-width t) (pict-height t) 3
					#:draw-border? #f)
				15
				#:color "indigo"
				#:shadow-color shadow-color)]
			[title-pict (inset (cc-superimpose background t) 20 30)]
		)
		title-pict))
(apply myslide #:title "Agenda"
	; #:ignore? #t
	(cut vc-append 20 <...>)
	(parameterize ([current-font-size 24]) (list
		'next (t "Introduction to Siteswap")
		'next (t "Equivalence")
		'next (t "Modification")
		'next (t "Modification Part 2: Possible Equivalence")
		'next (t "Future Work"))))
(myslide #:title "Intoduction to Siteswap"
	; #:ignore? #t
	vc-append
	(list* 'alt ct-superimpose
		(list 'tail (cut vl-append 20 <...>))
		(parameterize ([current-font-size 19]) (list
			(list 'map (cut vc-append
				(text "What is Siteswap?" (current-main-font) 26)
				<>))
			(blank 0 5)
			'next (t "Notation for the structure of juggling patterns")
			'next (t "Ignores flow")
			'next (t "Ignores flourishes")
			'next (t "Ignores exact timing and height")
			'next (t "Represents the relative timing")
			'next (t "Sequence of integers")
			)))
	(list 'alt ct-superimpose
		(list 'map (cut inset <> (* (beat-width) -9) 0 0 0))
		(list 'tail (λ ps (apply vc-append (reverse ps))))
		(list 'do (cut vc-append 10 <...>)
			(siteswap-numbers ss)
			'next
			(apply hb-append (beat-width)
				(for/list ([_ ss] [i (in-naturals)])
					(zero-width (text
						(if (= (modulo i 2) 0) "L" "R")
						(cons 'bold (current-main-font))
						16)))))
		'next
		(siteswap-arcs ss))
	ct-superimpose
	(parameterize ([current-font-size 19])
		(list 'do (cut vc-append 20 <>)
			(list 'map (cut ct-superimpose <>
				(blank 0 (+ (* (throw-height) 4) -10))))
			(t "Implicitly repeated infinitely")))
	; (list 'alt ct-superimpose (siteswap-numbers '(4 2 3)))
	; (list 'alt ct-superimpose (inset (siteswap-numbers '(4 2 3 4 2 3)) 0 0 (* (beat-width) -3) 0))
	; (list 'alt ct-superimpose (siteswap-numbers '(4 2 3 4 2 3 4 2 3)))
	; ct-superimpose
	(parameterize ([current-font-size 16])
		(list 'do vc-append
			(siteswap-numbers '(4 2 3))
			'next
			(list 'do (λ ps (apply vc-append (reverse ps)))
				(hb-append (beat-width)
					(zero-width (t "0"))
					(zero-width (t "1"))
					(zero-width (t "2")))
				'next
				(let ([p (zero-width (text " + "))])
					(inset
						(hb-append (beat-width) p p p)
						0 5 0 10)))
			'next
			(let ([p (zero-width (text " ≡ " (current-main-font) (current-font-size) (* -1/2 pi)))])
				(hc-append
					(inset
						(hb-append (beat-width) p p p)
						0 5 0 10)
					(zero-width (t "    mod 3") 0)))
			(hb-append (beat-width)
				(zero-width (t "1"))
				(zero-width (t "0"))
				(zero-width (t "2")))
			'next
			(blank 0 20)
			(text "Permutation!" (current-main-font) 18)
			'next
			(blank 0 20)
			(text "Disjoint cycle form: (01)(2)" (current-main-font) 18))))
(apply myslide #:title "Equivalence"
	; #:ignore? #t
	(cut vl-append 20 <...>)
	(list 'map (cut vc-append
		(text "When are two siteswaps equivalent?" (current-main-font) 26)
		<>))
	(blank 0 5)
	(parameterize ([current-font-size 19]) (list
		'next (t "Repetition")
		'next (list 'do hbl-append
			(list 'map (cut inset <> 30 0 0 0))
			(t "Example: 423 ≡ 423423")
			'next
			(t " ≡ 423423423"))
		'next (t "Scaling")
		'next (list 'do vl-append
			(list 'map (cut inset <> 30 0 0 0))
			(hbl-append (t "Choose a scaling factor ") (tex-math "k \\in \\mathbb{Z}^{+}"))
			(hbl-append (t "Multiply each throw by ") (tex-math "k"))
			(hbl-append (t "Add ") (tex-math "k - 1") (t " 0s after each throw"))
			'next
			(hbl-append (t "Example: ") (tex-math "k = 3") (t ", 423 → C00600900"))
			'next
			(hbl-append (tex-math "k") (t " must be odd, or it behaves weirdly")))
		)))
(apply myslide #:title "Modification"
	; #:ignore? #t
	(cut vl-append 20 <...>)
	(list 'map (cut vc-append
		(text "Are there easier ways to create siteswaps?" (current-main-font) 26)
		<>))
	(blank 0 5)
	(parameterize ([current-font-size 19]) (list
		'next (t "Swap")
		'next (list 'do vl-append
			(list 'map (cut inset <> 30 0 0 0))
			(t "Swap two consecutive throws")
			(t "Add one to the one moving left")
			(t "Subtract one from the one moving right")
			'next
			(t "Example: swap 2 and 3 in 423, produces 441"))
		'next (t "Extended Swap")
		'next (list 'do vl-append
			(list 'map (cut inset <> 30 0 0 0))
			(t "Generalizes swapping to non-consecutive throws")
			(t "Swap two throws")
			(t "Add the distance plus one to the one moving left")
			(t "Subtract the distance plus one from the one moving right")
			'next
			(t "Example: swap 4 and 3 in 423, produces 522"))
		'next (t "Add a ball")
		'next (list 'do vl-append
			(list 'map (cut inset <> 30 0 0 0))
			(t "Add the period to one of the throws")
			'next
			(t "Example: 423 → 453"))
		)))
(apply myslide #:title "Modification Part 2: Possible Equivalence"
	; #:ignore? #t
	(cut vl-append 20 <...>)
	(list 'map (cut vc-append
		(text "Are there easier ways to create siteswaps?" (current-main-font) 26)
		<>))
	(blank 0 5)
	(parameterize ([current-font-size 19]) (list
		'next (t "Removing 0s")
		'next (list 'do vl-append
			(list 'map (cut inset <> 30 0 0 0))
			(t "Remove any 0 from a pattern")
			(t "Subtract one from any throw which spans it")
			'next
			(t "Example: 44403 → 3333")
			'next
			(t "If you remove 2 consecutive 0s, it behaves nicely."))
		'next (t "Merging throws")
		'next (list 'do vl-append
			(list 'map (cut inset <> 30 0 0 0))
			(t "Pick a throw, replace it with 0")
			(t "Add the throw you removed to the throw which would land there")
			'next
			(t "Example: 531 → 630")
			'next
			(t "Can also do it in reverse."))
		)))
(myslide #:name "Future Work"
	; #:ignore? #t
	#:title "Future Work (What are these even‽)"
	vl-append
	(text "Things I want to investigate further:" 'default 23)
	(blank (- client-w 100) 20)
	(parameterize ([current-font-size 19])
		(list 'do (cut vl-append 20 <...>)
			(list 'map (cut inset <> 70 0 0 0))
			'next
			; no subsequence which can be duplicated?
			(t "Interesting infinite patterns.")
			'next
			(t "Why do patterns feel fast v.s. slow?")
			'next
			(t "Connection to group theory.")
			'next
			(inset
				(parameterize ([current-font-size 16])
					(hbl-append
						(t "What is an affine Weyl group of type ")
						(tex-math "{\\tilde A}_n")
						(t "?")))
				70 0 0 0)
			'next
			(t "Connection to category theory (and type systems for concatenative programming languages).")
				)))
