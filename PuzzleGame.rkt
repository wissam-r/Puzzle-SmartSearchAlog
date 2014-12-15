#lang racket

(struct point (x y)  #:transparent) ; a point ex : point 3 4
(struct line (m d p)   #:transparent) ;  a line m : mail ,  d : offset p :point
(struct limitedline (l p1 p2)  #:transparent) ; a limited line l :line p1 : point1 , p2 : point2 ex : line
(struct HighWidth (x y)  #:transparent)

(define (equalPoints p1 p2)(
	if (or (equal? p1 #f) (equal? p2 #f))
	#f
	(and (equal? (point-x p1) (point-x p2))
		(equal? (point-y p1) (point-y p2)))
	))

(equalPoints (point 1 2) (point 1 2)) ; given #t  true
(equalPoints (point 1 2) (point 1 3)) ; given #f  false


(define (equalLines l1 l2)(
	and (equal? (line-m l1) (line-m l2))
	(equal? (line-d l1) (line-d l2))
	))
(equalLines (line 1 2 (point 1 4)) (line 1 2 (point 1 4))) ; given #t  true 
(equalLines (line 1 2 (point 1 4)) (line 1 3 (point 1 5))) ; given #f  true 

(define (equalSpLines ll1 ll2)(
	and (equalLines (limitedline-l ll1) (limitedline-l ll2))
		(or (and (equalPoints (limitedline-p1 ll1) (limitedline-p1 ll2))
			(equalPoints (limitedline-p2 ll1) (limitedline-p2 ll2)))
			(and (equalPoints (limitedline-p2 ll1) (limitedline-p1 ll2))
			(equalPoints (limitedline-p1 ll1) (limitedline-p2 ll2)))
		)
	))

(equalSpLines (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2)) (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2))) ; given #t  true
(equalSpLines (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2)) (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 3))); given #f  true
(equalSpLines (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 2 2)) (limitedline (line 1 2 (point 1 4)) (point 2 2) (point 1 2))); given #t  true

(define (getLine p1 p2) (
	let* ([m (if (equal? (- (point-x p2) (point-x p1)) 0)
			"undefined"
			(/ (- (point-y p2) (point-y p1)) (- (point-x p2) (point-x p1))))]
		[d (if (equal? m "undefined")
			"undefined"
			(+ (* m (- 0 (point-x p1))) (point-y p1)))])
	(limitedline (line m d p1) p1 p2)))

(line-m (limitedline-l (getLine (point 1 1 ) (point 2 2 )))) ; given 1  true
(line-d (limitedline-l (getLine (point 1 1 ) (point 2 2 )))) ; given 0  true

(define (pointInLine l p) (
	if (equal? (line-m l) "undefined")
	(equal? (point-x (line-p l)) (point-x p))
	(equal? (point-y p) (+ (* (line-m l) (point-x p)) (line-d l)))
	))

(pointInLine (line 1 0 (point 1 1)) (point 1 1)) ; given #t  true
(pointInLine (line 1 0 (point 1 1)) (point 1 2)) ; given #f  true

(define (pointInSpicificLine ll p)(
	and (pointInLine (limitedline-l ll) p) (or 
		( <= ( * ( - (point-x p) (point-x (limitedline-p1 ll)))  
			( - (point-x p) (point-x (limitedline-p1 ll)))) 0 ) (> 1 0)))
	)

(pointInSpicificLine (getLine (point 1 1 ) (point 2 2 )) (point 1.5 1.5)) ; given #t  true
(pointInSpicificLine (getLine (point 1 1 ) (point 2 2 )) (point -1 1))  ; given #f  true

(define (match2Lines ll1  ll2)(
	equalSpLines ll1 ll2
	))

(define (halfMatch2LinesByStart ll1 ll2) (
	and (equalLines (limitedline-l ll1) (limitedline-l ll2))
		(equalPoints (limitedline-p1 ll1) (limitedline-p1 ll2))
	))

(halfMatch2LinesByStart (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 4)) (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2))) ; given #t  true
(halfMatch2LinesByStart (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 4)) (limitedline (line 1 2 (point 1 4)) (point 2 2) (point 1 2))) ; given #f  true

(define (halfMatch2LinesByEnd ll1 ll2) (
	and (equalLines (limitedline-l ll1) (limitedline-l ll2))
		(equalPoints (limitedline-p2 ll1) (limitedline-p2 ll2))
	))

(halfMatch2LinesByEnd (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 2 2)) (limitedline (line 1 2 (point 1 4)) (point 10 2) (point 2 2))) ; given #t  true
(halfMatch2LinesByEnd (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 2 2)) (limitedline (line 1 2 (point 1 4)) (point 10 2) (point 3 2))) ; given #f  true

(define (getIntersectionPoint l1 l2)(
	cond [(equalLines l1 l2) #f]
		[(equal? (line-m l1) "undefined") (point (point-x (line-p l1)) (+ (* (line-m l2) (point-x (line-p l1))) (line-d l2)))]
		[(equal? (line-m l2) "undefined") (point (point-x (line-p l2)) (+ (* (line-m l1) (point-x (line-p l2))) (line-d l1)))]
		[#t (let* ([x ( / ( - (line-d l2) (line-d l1)) (-(line-m l1) (line-m l2)))]
			[y (+ (* (line-m l1) x) (line-d l1))]
			)
			(point x y))]
	))

(point-x (getIntersectionPoint (line 1 1 (point 1 2)) (line -1 1 (point 1 0)))) ; given 0  true
(point-y (getIntersectionPoint (line 1 1 (point 1 2)) (line -1 1 (point 1 0)))) ; given 1 true
(getIntersectionPoint (line 1 1 (point 1 2)) (line -1 1 (point 1 0))) ; given ( point 0 1 ) ture

(define (endIntersection ll1 ll2 p) (
	if (equal? p #f)
	#f
	(and (pointInSpicificLine ll1 p) (pointInSpicificLine ll2 p) (equalPoints (limitedline-p2 ll2) p))
	))

(define limitedline-test1 (getLine (point 3 1) (point 0 1)))
(define limitedline-test2 (getLine (point 0 3) (point 0 0)))
(define limitedline-test3 (getLine (point 3 1) (point 0 1)))
(endIntersection limitedline-test2 limitedline-test3
	(getIntersectionPoint (limitedline-l limitedline-test2) (limitedline-l limitedline-test3)))
(endIntersection limitedline-test1 limitedline-test3
	(getIntersectionPoint (limitedline-l limitedline-test1) (limitedline-l limitedline-test3)))

(define (startIntersection ll1 ll2 p) (
	if (equal? p #f)
	#f
	(and (pointInSpicificLine ll1 p) (pointInSpicificLine ll2 p) (equalPoints (limitedline-p1 ll2) p))
	))

(define limitedline-test5 (getLine (point 0 1) (point 3 1)))
(define limitedline-test4 (getLine (point 0 3) (point 0 0)))

(startIntersection limitedline-test4 limitedline-test5
	(getIntersectionPoint (limitedline-l limitedline-test4) (limitedline-l limitedline-test5)))
(startIntersection limitedline-test5 limitedline-test4
	(getIntersectionPoint (limitedline-l limitedline-test5) (limitedline-l limitedline-test4)))

(define (outIntersection ll1 ll2 p) (
	if (equal? p #f)
	#f
	(and (pointInSpicificLine ll1 p) (pointInSpicificLine ll2 p) (not (equalPoints (limitedline-p2 ll2) p)))
	))
(define limitedline-test6 (getLine (point 0 1) (point 4 1)))
(define limitedline-test7 (getLine (point 0 3) (point 0 0)))

(outIntersection limitedline-test6 limitedline-test7
	(getIntersectionPoint (limitedline-l limitedline-test6) (limitedline-l limitedline-test7)))

(define (topLeftPoint shape maxPoint) (
	if (null? shape)
		maxPoint
		(if (null? maxPoint) 
			(let ([maxPoint (car shape)])
				(topLeftPoint (cdr shape) maxPoint)
				)
			(cond [(> (point-y (car shape)) (point-y maxPoint)) (topLeftPoint (cdr shape) (car shape))]
				[(and (equal? (point-y (car shape)) (point-y maxPoint)) (< (point-x (car shape)) (point-x maxPoint)))
					 (topLeftPoint (cdr shape) (car shape))]
				[#t (topLeftPoint (cdr shape) maxPoint)]
					 )
			)
	))
(topLeftPoint (list (point 1 1) (point 1 0) (point 0 0) (point 0 1) ) null )